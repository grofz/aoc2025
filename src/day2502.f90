  module day2502_mod
    use iso_fortran_env, only : i8 => int64
    implicit none
    private
    public day2502

  contains

    subroutine day2502(file)
      character(len=*), intent(in) :: file

      character(len=10000) :: line
      integer :: i,j
      integer(i8) :: a, b, c, ans1, ans2

      open(10,file=file,status='old')
      read(10,'(a)') line
      close(10)
      i = 1
      ans1 = 0
      ans2 = 0
      do
        j = i-1 + index(line(i:),',')         ! position of next "," on line
        if (j==i-1) j = len_trim(line)+1      ! no more ","
        call parse_range(line(i:j-1), a, b)   ! interval "a-b"
        do c = a, b
          if (is_invalid(int2str(c))) ans1 = ans1 + c
          if (is_invalid_p2(int2str(c))) ans2 = ans2 + c
        end do

        if (j==len_trim(line)+1) exit
        i = j + 1                             ! continue just behind ","
      end do
      print '("02/1: ",i0,1x,l)', ans1, ans1==19219508902_i8
      print '("02/2: ",i0,1x,l)', ans2, ans2==27180728081_i8
    end subroutine day2502


    subroutine parse_range(word, a, b)
      character(len=*), intent(in) :: word
      integer(i8), intent(out) :: a, b
      integer :: i
      i = index(word,'-')
      if (i==0) error stop 'no hyphen found'
      read(word(:i-1),*) a
      read(word(i+1:),*) b
    end subroutine


    function int2str(a) result(str)
      integer(i8), intent(in) :: a
      character(len=:), allocatable :: str
      character(len=100) line

      write(line,*) a
      line = adjustl(line)
      str = trim(line) ! auto-allocation
    end function


    logical function is_invalid(str) result(invalid)
      character(len=*), intent(in) :: str
      invalid = .false.
      if (mod(len(str),2)==0) &
          & invalid = str(:len(str)/2) == str(len(str)/2+1:)
    end function


    logical function is_invalid_p2(str) result(invalid)
      character(len=*), intent(in) :: str
      integer :: patterns, pattern_len, j, j0, j1, j2

      invalid = .false.
      MAIN: do patterns = 2, len(str)
        if (mod(len(str),patterns)==0) then
          pattern_len = len(str)/patterns
          do j=1, patterns-1
            j0 = (j-1)*pattern_len + 1
            j1 = (j  )*pattern_len + 1
            j2 = (j+1)*pattern_len + 1
            if (str(j0:j1-1)/=str(j1:j2-1)) cycle MAIN
          end do
          ! all patterns are same
          invalid = .true.
          exit MAIN
        end if
      end do MAIN
    end function

  end module day2502_mod
