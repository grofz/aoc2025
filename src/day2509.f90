  module day2509_mod
    use iso_fortran_env, only : i8 => int64
    use parse_mod, only : string_t, read_strings

    type edge_v_t
      integer(i8) :: x, y1, y2
    end type edge_v_t

  contains

    subroutine day2509(file)
      character(len=*), intent(in) :: file

      integer(i8), allocatable :: xy(:,:)
      integer(i8) :: ans1, ans2, a, x1, x2, y1, y2
      integer :: i, j, k
      type(edge_v_t), allocatable :: vlist(:)

      call read_xy(file, xy)
      vlist = vertical_list(xy)
      ans1 = 0
      ans2 = 0
      do i=1, size(xy,2)-1
        do j=i+1, size(xy,2)
          x1 = min(xy(1,i), xy(1,j))
          y1 = min(xy(2,i), xy(2,j))
          x2 = max(xy(1,i), xy(1,j))
          y2 = max(xy(2,i), xy(2,j))
          a = (x2-x1+1)*(y2-y1+1)
          if (a > ans1) ans1 = a

          ! check no other point is inside the rectangle
          do k=1, size(xy,2)
            if (k==i .or. k==j) cycle
            if (xy(1,k)>x1 .and. xy(1,k)<x2 .and. xy(2,k)>y1 .and. xy(2,k)<y2) then
              a = 0
              exit
            end if
          end do
          if (a==0) then
            ! other red is inside proposed area
          else
            ! check the remaining corners are inside
            if (is_inside([x1,y1],vlist) .and. is_inside([x1,y2],vlist) &
              .and. is_inside([x2,y1],vlist) .and. is_inside([x2,y2],vlist)) &
              then
              print '("a=",i0," p1=",i0,1x,i0," p2=",i0,1x,i0)', a, x1,y1,x2,y2
              if (a > ans2) then
                ans2 = a
                print *, 'this is max '
              end if
            end if
          end if
        end do
      end do

      print '("09/1: ",i0,l2)', ans1, ans1==4745816424_i8
      print '("09/1: ",i0,l2)', ans2, ans2==4661665300_i8 ! too high
    end subroutine day2509


    function vertical_list(xy) result(v)
      integer(i8), intent(in) :: xy(:,:)
      type(edge_v_t), allocatable :: v(:)

      integer :: i, j
      type(edge_v_t) :: newv
      allocate(v(0))

      do i=1, size(xy,2)
        j = i+1
        if (j>size(xy,2)) j =1
        if (xy(1,i)==xy(1,j)) then
          ! vertical line
          newv%x = xy(1,i)
          newv%y1 = min(xy(2,i),xy(2,j))
          newv%y2 = max(xy(2,i),xy(2,j))
          v = [v, newv]
        end if
      end do
    end function vertical_list


    logical function is_inside(xy, vlist)
      integer(i8), intent(in) :: xy(2)
      type(edge_v_t), intent(in) :: vlist(:)

      integer :: nhits, i

      is_inside = .false.
      nhits = 0
      do i=1, size(vlist)
        ! check if it is one of end points
        if ((xy(1)==vlist(i)%x) .and. &
            (xy(2)==vlist(i)%y1 .or. xy(2)==vlist(i)%y2)) then
            is_inside = .true.
          exit
        end if

        if (xy(2) >  vlist(i)%y2) cycle
        if (xy(2) <= vlist(i)%y1) cycle
        if (xy(1) == vlist(i)%x) then
          is_inside = .true.
          exit
        end if
        if (xy(1) < vlist(i)%x) nhits = nhits+1
      end do
      if (.not. is_inside) is_inside = mod(nhits,2)==1
    end function is_inside


    subroutine read_xy(file, xy)
      character(len=*), intent(in) :: file
      integer(i8), allocatable, intent(out) :: xy(:,:)

      type(string_t), allocatable :: lines(:)
      integer :: i, j

      lines = read_strings(file)
      allocate(xy(2, size(lines)))
      do i=1, size(lines)
        j = index(lines(i)%str,',')
        if (j==0) error stop 'comma not found'
        read(lines(i)%str(:j-1),*) xy(1,i)
        read(lines(i)%str(j+1:),*) xy(2,i)
      end do
    end subroutine read_xy

  end module day2509_mod
