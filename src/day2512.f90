module day2512_mod
  use parse_mod, only : read_strings, string_t, split_nonempty
  implicit none

  integer, parameter :: nx=3, ny=3

contains

  subroutine day2512(file)
    character(len=*), intent(in) :: file

    integer, allocatable :: a(:,:), b(:,:,:), c(:,:)
    integer :: ans, i

    call top_level_parse(file, a, b, c)
    ans = 0
    do i=1, size(c,2)
      block
        integer :: req_max, req_min, ava, j
        req_max = nx*ny*sum(a(:,i))
        req_min = 0
        do j=1, size(a,1)
          req_min = req_min + a(j,i)*sum(b(:,:,j))
        end do
        ava = c(1,i)*c(2,i)
        if (req_max <= ava) then
          ! packets fit as 3x3 blocks
          ans = ans + 1
        else if (req_min > ava) then
          ! impossible to fit
          continue
        else
          ! this case requires attention
          print '("req ",i0,1x,i0,":  ava ",i0)', req_min, req_max, ava
        end if
      end block
    end do
    print '("12:  ",i0,l2)', ans, ans==440
  end subroutine day2512


  subroutine top_level_parse(file, a, b, c)
    character(len=*), intent(in) :: file
    integer, intent(out), allocatable :: a(:,:), b(:,:,:), c(:,:)

    type(string_t), allocatable :: lines(:)
    integer :: i, j, k, na, nb, ib

    ! First pass
    lines = read_strings(file)
    na = 0
    nb = 0
    do i=1,size(lines)
      j = index(lines(i)%str,':')
      if (j==0) cycle
      j = index(lines(i)%str(:j-1),'x')
      if (j==0) then
        nb = nb + 1 ! package definition
      else
        na = na + 1 ! tree area specification
      end if
    end do

    allocate(a(nb, na), source=-1)   ! requested number of packages
    allocate(b(nx,ny,nb), source=-1) ! package shape
    allocate(c(2,na), source=-1)     ! tree area size

    ! Second pass - package shapes
    do i=1, (nb-1)*(ny+2)+1, ny+2
      j = index(lines(i)%str,':')
      if (j==0) error stop 'parse 1'
      read(lines(i)%str(:j-1),*) ib
      do j=1,ny
        do k=1, nx
          if (lines(i+j)%str(k:k)=='#') then
            b(j,k,ib+1) = 1
          else if (lines(i+j)%str(k:k)=='.') then
            b(j,k,ib+1) = 0
          else
            error stop 'parse 2'
          end if
        end do
      end do
    end do

    ! Third pass - areas
    ib = 1
    do i=nb*(ny+2)+1, size(lines)
      j = index(lines(i)%str,':')
      if (j==0) error stop 'parse 3'
      k = index(lines(i)%str(:j-1),'x')
      if (k==0) error stop 'parse 4'
      read(lines(i)%str(:k-1),*) c(1,ib)
      read(lines(i)%str(k+1:j-1),*) c(2,ib)
      read(lines(i)%str(j+1:),*) a(:,ib)
      ib = ib + 1
    end do

    return
    do i=1, size(a,2)
      print '("Dim =",i0,1x,i0,"  Counts =",*(i0,1x))', c(:,i), a(:,i)
    end do
    do i=1, size(b,3)
      print *
      do j = 1, size(b, 1)
        print *, b(j,:,i)
      end do
    end do
  end subroutine top_level_parse
end module day2512_mod