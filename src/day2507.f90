module day2507_mod
  use parse_mod, only : string_t, read_strings
  use iso_fortran_env, only : i8 => int64
  implicit none
  private
  public day2507

  character(len=1), parameter :: C_SPACE='.', C_SPLITTER='^', C_BEAM='|', C_START='S'
contains

  subroutine day2507(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: aa(:,:)
    integer(i8), allocatable :: bb(:,:)
    integer :: i, ans1
    integer(i8) :: ans2

    ! input can be considered as two-lines blocks, first line contains
    ! splitters/strarting-node, the second line just spaces
    aa = read_input(file)
    ! only even rows are relevant, but we keep dimesnions same as for
    ! the input to make life easier
    allocate(bb(size(aa,1),size(aa,2)), source=0_i8)

    ans1 = 0
    do i=3,size(aa,1),2
      call one_block(i, aa, ans1, bb)
    end do
   !do i=1, size(aa,1)
   !  print '(*(a1))', aa(i,:)
   !end do
    ans2 = sum(bb(size(bb,1),:))

    print '("07/1: ", i0, 1x, l)', ans1, ans1==1507
    print '("07/2: ", i0, 1x, l)', ans2, ans2==1537373473728_i8
  end subroutine day2507


  subroutine one_block(i, aa, cnt_splitters, bb)
    integer, intent(in) :: i
    character(len=1), intent(inout) :: aa(:,:)
    integer(i8), intent(inout) :: bb(:,:)
    integer, intent(inout) :: cnt_splitters

    integer :: j

    if (mod(i,2)/=1) error stop 'i must be odd'
    do j=1, size(aa,2)
      if (aa(i,j)==C_START) then
        aa(i+1,j) = C_BEAM
        bb(i+1,j) = 1
      else
        if (aa(i-1,j)==C_BEAM) then
          select case(aa(i,j))
          case(C_SPACE)
            aa(i+1,j) = C_BEAM
            bb(i+1,j) = bb(i+1,j) + bb(i-1,j)
          case(C_SPLITTER)
            aa(i+1,j-1) = C_BEAM
            aa(i+1,j+1) = C_BEAM
            bb(i+1,j-1) = bb(i+1,j-1) + bb(i-1,j)
            bb(i+1,j+1) = bb(i+1,j+1) + bb(i-1,j)
            cnt_splitters = cnt_splitters + 1
          case default
            error stop 'unreachable reached'
          end select
        end if
      end if
    end do
  end subroutine one_block


  function read_input(file) result(aa)
    character(len=*), intent(in) :: file
    character(len=1), allocatable :: aa(:,:)

    type(string_t), allocatable :: lines(:)
    integer :: ncol, nrow, i, j

    lines = read_strings(file)
    ncol = len_trim(lines(1)%str)
    nrow = size(lines)

    ! assert even number of rows
    if (mod(nrow,2)/=0) error stop 'not even number of rows'
    ! we put an empty double-rows above the input
    allocate(aa(-1:nrow,1:ncol), source=C_SPACE)

    do i=1,nrow
      if (len_trim(lines(i)%str)/=ncol) error stop 'not same length of all lines'
      if (mod(i,2)==0) then
        if (repeat(C_SPACE, ncol)/=lines(i)%str) error stop 'even lines not all spaces'
      end if
      if (verify(lines(i)%str, C_BEAM//C_SPACE//C_SPLITTER//C_START)/=0) &
          & error stop 'unexpected character'
      do j=1, ncol
        aa(i,j)=lines(i)%str(j:j)
      end do
    end do
  end function read_input

end module day2507_mod