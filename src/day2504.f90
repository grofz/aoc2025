  module day2504_mod
    use parse_mod, only : string_t, read_strings
    implicit none

    character(len=1), parameter :: C_ROLL = '@', C_FREE = '.'
  contains
    subroutine day2504(file)
      character(len=*), intent(in) :: file

      character(len=1), allocatable, dimension (:,:) :: field
      integer :: ans1, ans2, i

      call read_field(file, field)
      call remove_rolls(field, ans1)
      ans2 = ans1
      do
        call remove_rolls(field, i)
        if (i==0) exit
        ans2 = ans2 + i
      end do
      print '("04/1: ",i0,1x,l1)', ans1, ans1==1493
      print '("04/2: ",i0,1x,l1)', ans2, ans2==9194
    end subroutine day2504


    subroutine remove_rolls(a, removed)
      character(len=1), intent(inout) :: a(0:,0:)
      integer, intent(out) :: removed

      integer, allocatable, dimension(:,:) :: ngbs
      integer :: i

      ngbs = count_ngbs(a)

      removed = count(ngbs<4 .and. a(1:ubound(a,1)-1,1:ubound(a,2)-1)==C_ROLL)
      where (ngbs<4 .and. a(1:ubound(a,1)-1,1:ubound(a,2)-1)==C_ROLL)
        a(1:ubound(a,1)-1, 1:ubound(a,2)-1) = C_FREE
      end where
    end subroutine remove_rolls


    function count_ngbs(a) result(ngbs)
      character(len=1), intent(in) :: a(0:,0:)
      integer, allocatable :: ngbs(:,:)

      integer :: i, j, di, dj, ni, nj

      ni = size(a,1)-2
      nj = size(a,2)-2
      allocate(ngbs(ni,nj), source=0)

      do i=1,ni
        do j=1,nj
          do di=-1,1
            do dj=-1,1
              if (di==0 .and. dj==0) cycle
              if (a(i+di,j+dj)==C_ROLL) ngbs(i,j) = ngbs(i,j) + 1
            end do
          end do
        end do
      end do
    end function count_ngbs


    subroutine read_field(file, field)
      character(len=*), intent(in) :: file
      character(len=1), allocatable, dimension (:,:), intent(out) :: field

      type(string_t), allocatable :: lines(:)
      integer :: ncol, nrow, i, j

      lines = read_strings(file)
      nrow = size(lines)
      ncol = len_trim(lines(1)%str)

      allocate(field(0:nrow+1, 0:ncol+1), source=C_FREE)
      do i=1, nrow
        if (len_trim(lines(i)%str) /= ncol) &
            error stop 'some lines have different length'
        do j=1, ncol
          field(i,j) = lines(i)%str(j:j)
        end do
      end do
     !print '("Field of ",i0," lines and ",i0," columns")', nrow, ncol
     !print '("Rolls = ",i0,"  Free = ",i0)', &
     !    count(field(1:nrow,1:ncol)==C_ROLL), &
     !    count(field(1:nrow,1:ncol)==C_FREE)
    end subroutine read_field

  end module day2504_mod
