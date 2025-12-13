module day2511_mod
  use parse_mod, only : read_strings, string_t, split_nonempty
  use iso_fortran_env, only : i8 => int64
  implicit none

  integer, parameter :: LABEL_LEN=3, EXIT_IND=0
  integer(i8), parameter :: UNKNOWN=-1
  character(len=*), parameter :: EXIT_LABEL='out'

  type vertex_t
    character(len=LABEL_LEN) :: label
    integer, allocatable :: ngbs(:)
  end type

contains

  subroutine day2511(file)
    character(len=*), intent(in) :: file

    type(vertex_t), allocatable :: vv(:)
    integer(i8) ::  ans1, ans2(3)
    integer ::  i

    vv = read_network(file)
    do i=1,size(vv)
   !  print '(i4,". ",a,": ",*(i4))', i, vv(i)%label, vv(i)%ngbs
    end do

    ! Part 1
    call crawl_dfs(vv, find_label(vv, 'you'), EXIT_IND, ans1, .true.)

    ! Part 2
    associate(dac => find_label(vv, 'dac'), fft => find_label(vv, 'fft'))
      call crawl_dfs(vv, find_label(vv, 'svr'), fft, ans2(1), .true.)
      call crawl_dfs(vv, fft, dac, ans2(2), .true.)
      call crawl_dfs(vv, dac, EXIT_IND, ans2(3), .true.)
    end associate

    print '("11/1:",i0,l2)', ans1, ans1==696
    print '("11/2:",i0,l2)', product(ans2), product(ans2)==473741288064360_i8
  end subroutine day2511


  recursive subroutine crawl_dfs(vv, where, target, count, main_level)
    type(vertex_t), intent(in) :: vv(:)
    integer, intent(in) :: where, target
    integer(i8), intent(out) :: count
    logical, optional :: main_level

    integer :: i
    integer(i8) :: loc_count
    integer(i8), allocatable, save :: memo(:) ! global

    ! Reset memoization table if in main level
    if (present(main_level)) then
      if (main_level .and. allocated(memo)) deallocate(memo)
    end if
    if (.not. allocated(memo)) allocate(memo(0:size(vv)), source=UNKNOWN)

    ! Using memoized value
    if (memo(where)/=UNKNOWN) then
      count = memo(where)
      return
    end if

    ! Calculation of yet unknown value
    if (where==target) then
      count = 1
    else if (where==EXIT_IND) then
      ! must prune search if end node reached
      count = 0
    else
      ! recursivelly continue at all neighbors 
      count = 0
      do i=1, size(vv(where)%ngbs)
        call crawl_dfs(vv, vv(where)%ngbs(i), target, loc_count)
        count = count + loc_count
      end do
    end if
    memo(where) = count
  end subroutine crawl_dfs


  function read_network(file) result(vv)
    character(len=*), intent(in) :: file
    type(vertex_t), allocatable :: vv(:)

    type(string_t), allocatable :: lines(:), ngbs(:)
    integer :: i, j

    lines = read_strings(file)
    allocate(vv(size(lines)))
    ! first pass - read labels
    do i=1, size(vv)
      j = index(lines(i)%str,':')
      if (j/=LABEL_LEN+1) error stop 'day 11 parsing error'
      vv(i)%label = lines(i)%str(1:LABEL_LEN)
    end do

    ! second pass - read connections
    do i=1, size(vv)
      allocate(vv(i)%ngbs(0))
      call split_nonempty(lines(i)%str(LABEL_LEN+2:), ' ', ngbs)
      do j=1, size(ngbs)
        if (len(ngbs(j)%str)/=LABEL_LEN) error stop 'day 11 wrong ngb len'
        vv(i)%ngbs = [vv(i)%ngbs, find_label(vv, ngbs(j)%str)]
      end do
      if (size(vv(i)%ngbs)<1) error stop 'no neighbors found'
    end do
  end function read_network


  pure function find_label(vv, label) result(ind)
    type(vertex_t), intent(in) :: vv(:)
    character(len=LABEL_LEN), intent(in) :: label
    integer :: ind

    integer :: i

    if (label==EXIT_LABEL) then
      ind = EXIT_IND
      return
    end if
    do i=1, size(vv)
      if (vv(i)%label==label) exit
    end do
    ind = i
    if (ind == size(vv)+1) error stop 'could not find label '//label
  end function find_label

end module day2511_mod