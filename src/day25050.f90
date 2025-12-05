module day2505_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : i8 => int64
  implicit none

  type range_t
    integer(i8) :: ibeg, iend
  contains
    procedure :: is_fresh, is_valid
  end type
  interface range_t
    module procedure parse_range
  end interface

contains

  subroutine day2505(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(range_t), allocatable :: ranges(:)
    integer :: i, j, i_start_of_list
    integer :: ans1
    integer(i8) :: ans2, item

    ! count the number of intervals and food items
    lines = read_strings(file)
    do i_start_of_list=1,size(lines)
      if (lines(i_start_of_list)%str=='') exit
    end do

    ! parse intervals and sort them by their starting value
    allocate(ranges(i_start_of_list-1))
    do i=1, size(ranges)
      ranges(i) = range_t(lines(i)%str)
    end do
    call sort_ranges(ranges)

    ! part 1
    ans1 = 0
    do i=i_start_of_list+1, size(lines)
      read(lines(i)%str,*) item
      do j=1, size(ranges)
        if (ranges(j)%is_fresh(item)) then
          ans1 = ans1 + 1
          exit
        end if
      end do
    end do

    ! part 2
    ! Remove overlaps
    do i=1, size(ranges)
      if (.not. ranges(i)%is_valid()) cycle
      ! shift the starting point of remaining intervals behind the end point
      ! of the current interval. Intervals that become invalidated (become zero length)
      ! by this operation will be ignored (instead of removing them from array)
      do j=i+1, size(ranges)
        if (.not. ranges(j)%is_valid()) cycle
        if (ranges(j)%ibeg <= ranges(i)%iend) then
          ranges(j)%ibeg = ranges(i)%iend + 1
        end if
      end do
    end do

    ! The intervals no longer overlap, sum their total length
    ans2 = 0
    do i=1, size(ranges)
      if (ranges(i)%is_valid()) ans2 = ans2 + ranges(i)%iend-ranges(i)%ibeg+1
    end do

    ! Summary
    print '("05/1: ",i0,1x,l1)', ans1, ans1==517
    print '("05/2: ",i0,1x,l1)', ans2, ans2==336173027056994_i8

  end subroutine day2505


  subroutine sort_ranges(arr)
    type(range_t), intent(inout) :: arr(:)

    type(range_t) :: tmp
    integer :: i, j

    ! selection sort
    do i = 2, size(arr)
      tmp = arr(i)
      j = i-1
      do 
        if (j < 1) exit
        if (arr(j)%ibeg <= tmp%ibeg) exit
        arr(j+1) = arr(j)
        j = j-1
      end do
      arr(j+1) = tmp
    end do
  end subroutine sort_ranges


  pure logical function is_fresh(this, val)
    class(range_t), intent(in) :: this
    integer(i8), intent(in) :: val
    is_fresh = val >= this%ibeg .and. val <= this%iend
  end function


  pure logical function is_valid(this)
    class(range_t), intent(in) :: this
    is_valid = this%iend >= this%ibeg
  end function


  type(range_t) function parse_range(str) result(new)
    character(len=*), intent(in) :: str

    type(string_t), allocatable :: items(:)

    call split(str, '-', items)
    if (size(items)/=2) error stop 'cannot parse range'
    read(items(1)%str,*) new%ibeg
    read(items(2)%str,*) new%iend
  end function parse_range

end module day2505_mod