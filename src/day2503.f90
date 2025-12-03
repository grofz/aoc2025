module day2503_mod
  use parse_mod, only : string_t, read_strings
  use iso_fortran_env, only : i8 => int64
  implicit none
  private
  public day2503

contains

  subroutine day2503(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer, allocatable :: arr(:), pos(:)
    integer :: i, j
    integer(i8) :: ans1, ans2

    ans1 = 0
    ans2 = 0
    lines = read_strings(file)
    do i=1, size(lines)
      arr = line2array(lines(i)%str)
      ! Part 1
      call get_position(arr, 2, pos)
      ans1 = ans1 + evaluate_position(arr, pos)
      ! Part 2
      call get_position(arr, 12, pos)
      ans2 = ans2 + evaluate_position(arr, pos)
!     print '(*(i1))', (arr(pos(j)), j=1,ubound(pos,1))
    end do
    print '("03/1: ",i0,1x,l)', ans1, ans1 == 17330
    print '("03/2: ",i0,1x,l)', ans2, ans2 == 171518260283767_i8
  end subroutine day2503


  pure subroutine get_position(arr, nbank, pos)
    integer, intent(in) :: arr(:), nbank
    integer, intent(out), allocatable :: pos(:)
!
! Return positions of batteries that should be switched on to maximize
! the joltage.
!
    integer :: j, last_pos

    allocate(pos(nbank))
    last_pos = 0
    do j=1,nbank
      pos(j) = find_position_of_max(arr, last_pos, size(arr)-(nbank-j))
      last_pos = pos(j)
    end do
  end subroutine get_position


  pure function evaluate_position(arr, pos) result(n)
    integer, intent(in) :: arr(:), pos(:)
    integer(i8) :: n
!
! Convert switched-on digits to the number
!
    integer :: i

    n = 0
    do i=1, ubound(pos,1)
      n = n + arr(pos(i)) * 10_i8**(ubound(pos,1)-i)
    end do
  end function evaluate_position


  pure function find_position_of_max(arr, ibeg, iend) result(pos)
    integer, intent(in) :: arr(:), ibeg, iend
    integer :: pos
!
! Find the left-most position of the largest digit in the sub-array
! "arr(ibeg+1:iend)"
!
    integer :: i, j

    do j=9, 1, -1
      do i=ibeg+1, iend
        if (arr(i)==j) then
          pos = i
          return
        end if
      end do
    end do
    error stop 'should not get here...'
  end function find_position_of_max


  pure function line2array(line) result(arr)
    character(len=*), intent(in) :: line
    integer, allocatable :: arr(:)
!
! The line contains string of digits between 1 and 9
!
    integer :: i
    allocate(arr(len(line)))
    do i=1, size(arr)
      arr(i) = iachar(line(i:i))-iachar('0')
    end do
  end function line2array

end module day2503_mod