module day2506_mod
  use parse_mod, only : split_nonempty, string_t, read_strings
  use iso_fortran_env, only : i8 => int64
  implicit none

contains

  subroutine day2506(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(string_t), allocatable :: ops(:), nums(:)
    integer(i8), allocatable :: arr(:,:), colnums(:)
    character(len=1), allocatable :: digits(:,:)
    integer(i8) :: ans1, ans2, tmp
    integer :: i, j, k, cols

    ! read all lines
    lines = read_strings(file)
    cols = 0 ! max line length
    do i=1, size(lines)
      if (len_trim(lines(i)%str)> cols) cols = len_trim(lines(i)%str)
    end do

    ! array of operators
    call split_nonempty(lines(size(lines))%str, ' ', ops)

    ! left to right read numbers for part 1
    allocate(arr(size(lines)-1, size(ops)))

    ! top to bottom read numbers for part 2 
    allocate(digits(size(lines)-1, cols), source=' ')
    allocate(colnums(cols), source=0_i8)

    do i=1, size(lines)-1
      ! part 1
      call split_nonempty(lines(i)%str,' ',nums)
      do j=1, size(nums)
        read(nums(j)%str, *) arr(i,j)
      end do

      ! part 2
      do j=1, cols
        if (j<=len_trim(lines(i)%str)) digits(i,j)=lines(i)%str(j:j)
      end do
    end do

    ! part 2 - each column is a number or zero for an empty column
    do j=1, cols
      k = -1
      do i=size(digits,1),1,-1
        if (digits(i,j)==' ') cycle
        k = k+1
        read(digits(i,j),*) tmp
        colnums(j) = colnums(j) + tmp*10**k
      end do
    end do

    ! now do the actual math
    ans1 = 0
    ans2 = 0
    k = 0 ! column index
    do j=1, size(ops)
      ! part 1
      tmp = init_operation(ops(j)%str)
      do i=1, size(lines)-1
        call do_operation(ops(j)%str, tmp, arr(i,j))
      end do
      ans1 = ans1 + tmp

      ! Part 2
      tmp = init_operation(ops(j)%str)
      do
        k = k + 1
        if (k>size(colnums)) exit
        if (colnums(k)==0) exit
        call do_operation(ops(j)%str, tmp, colnums(k))
      end do
      ans2 = ans2 + tmp
    end do

    print '("06/1: ",i0,1x,l)', ans1, ans1==4805473544166_i8
    print '("06/1: ",i0,1x,l)', ans2, ans2==8907730960817_i8
  end subroutine day2506


  subroutine do_operation(op, a, b)
    character(len=1), intent(in) :: op
    integer(i8), intent(inout) :: a
    integer(i8), intent(in) :: b
    select case(op)
    case('+')
      a = a + b
    case('*')
      a = a * b
    case default
      error stop 'invalid op'
    end select
  end subroutine


  function init_operation(op) result(num)
    character(len=1), intent(in) :: op
    integer(i8) :: num
    select case(op)
    case('+')
      num = 0
    case('*')
      num = 1
    case default
      error stop 'invalid op'
    end select
  end function

end module day2506_mod