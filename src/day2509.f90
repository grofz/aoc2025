  module day2509_mod
    use iso_fortran_env, only : i8 => int64
    use parse_mod, only : string_t, read_strings

    type edge_t
      integer(i8) :: x1, x2, y1, y2
    contains
      procedure :: isvertical => edge_isvertical
    end type edge_t

    type box_t
      integer(i8) :: x1, x2, y1, y2
    contains
      procedure :: area => box_area, inside => box_inside
      procedure :: left, right, top, bottom
    end type box_t
    interface box_t
      module procedure box_new
    end interface

  contains

    subroutine day2509(file)
      character(len=*), intent(in) :: file

      integer(i8), allocatable :: xy(:,:)
      integer(i8) :: ans1, ans2, a, x1, x2, y1, y2
      integer :: i, j, k
      type(edge_t), allocatable :: elist(:)
      type(box_t) :: box

      call read_xy(file, xy)
      elist = edge_list(xy)
      ans1 = 0
      ans2 = 0
      do i=1, size(xy,2)-1
        do j=i+1, size(xy,2)
          box = box_t(xy(:,i), xy(:,j))
          a = box%area()
          if (a > ans1) ans1 = a

          ! check no other point is inside the rectangle
          if (box%inside(elist)) then
            if (a > ans2) then
              ans2 = a
              print *, 'this is max '
            end if
          end if
        end do
      end do

      print '("09/1: ",i0,l2)', ans1, ans1==4745816424_i8
      print '("09/1: ",i0,l2)', ans2, ans2==4661665300_i8 ! too high
    end subroutine day2509


    function box_inside(this, elist) result(is)
      class(box_t), intent(in) :: this
      type(edge_t), intent(in) :: elist(:)
      logical :: is

      ! all its corners are inside polygon
      is = is_inside([this%x1,this%y1], elist) .and. &
           is_inside([this%x1,this%y2], elist) .and. &
           is_inside([this%x2,this%y1], elist) .and. &
           is_inside([this%x2,this%y2], elist)
      if (.not. is) return

      ! no box side cross a touch edge in its middle
      is = .not. (edge_cross(this%left(),   elist) .or. &
                  edge_cross(this%right(),  elist) .or. &
                  edge_cross(this%top(),    elist) .or. &
                  edge_cross(this%bottom(), elist) )
    end function box_inside


    logical function edge_cross(a, elist) result(iscrossing)
      type(edge_t), intent(in) :: a, elist(:)

      integer :: i

      iscrossing = .false.

      do i=1,size(elist)
        associate(b=>elist(i))
        if (a%isvertical() .eqv. b%isvertical()) cycle
        if (a%isvertical()) then
          if (b%y1 > a%y1 .and. b%y1 < a%y2 .and. &
              a%x1 >= b%x1 .and. a%x1 <= b%x2) then
            iscrossing = .true.
          end if
        else ! a is horizontal
          if (b%x1 > a%x1 .and. b%x1 < a%x2 .and. &
              a%y1 >= b%y1 .and. a%y1 <= b%y2) then
            iscrossing = .true.
          end if
        end if
        if (iscrossing) exit
        end associate
      end do

    end function edge_cross


    function box_new(p1, p2) result(new)
      integer(i8), intent(in) :: p1(2), p2(2)
      type(box_t) :: new

      new%x1 = min(p1(1), p2(1))
      new%y1 = min(p1(2), p2(2))
      new%x2 = max(p1(1), p2(1))
      new%y2 = max(p1(2), p2(2))
    end function box_new


    function box_area(this) result(a)
      class(box_t), intent(in) :: this
      integer(i8) :: a

      a = (this%x2-this%x1+1) * (this%y2-this%y1+1)
    end function box_area


    function left(this) result(edge)
      class(box_t), intent(in) :: this
      type(edge_t) :: edge
      edge = edge_t(x1=this%x1, x2=this%x1, y1=this%y1, y2=this%y2)
    end function


    function right(this) result(edge)
      class(box_t), intent(in) :: this
      type(edge_t) :: edge
      edge = edge_t(x1=this%x2, x2=this%x2, y1=this%y1, y2=this%y2)
    end function


    function top(this) result(edge)
      class(box_t), intent(in) :: this
      type(edge_t) :: edge
      edge = edge_t(x1=this%x1, x2=this%x2, y1=this%y1, y2=this%y1)
    end function


    function bottom(this) result(edge)
      class(box_t), intent(in) :: this
      type(edge_t) :: edge
      edge = edge_t(x1=this%x1, x2=this%x2, y1=this%y2, y2=this%y2)
    end function


    function edge_list(xy) result(v)
      integer(i8), intent(in) :: xy(:,:)
      type(edge_t), allocatable :: v(:)

      integer :: i, j
      type(edge_t) :: newv
      allocate(v(0))

      do i=1, size(xy,2)
        j = i+1
        if (j>size(xy,2)) j =1
        if (xy(1,i)==xy(1,j)) then
          ! vertical line
        else if (xy(2,i)==xy(2,j)) then
          ! horizontal line
        else
          error stop 'line nor horizontal or vertical'
        end if
        newv%x1 = min(xy(1,i),xy(1,j))
        newv%x2 = max(xy(1,i),xy(1,j))
        newv%y1 = min(xy(2,i),xy(2,j))
        newv%y2 = max(xy(2,i),xy(2,j))
        v = [v, newv]
      end do
    end function edge_list


    function edge_isvertical(this) result(is)
      class(edge_t), intent(in) :: this
      logical :: is

      is = this%x1 == this%x2
    end function edge_isvertical


    logical function is_inside(xy, vlist)
      integer(i8), intent(in) :: xy(2)
      type(edge_t), intent(in) :: vlist(:)

      integer :: nhits, i

      is_inside = .false.
      nhits = 0
      do i=1, size(vlist)
        if (.not. vlist(i)%isvertical()) cycle
        ! check if it is one of end points
        if ((xy(1)==vlist(i)%x1) .and. &
            (xy(2)==vlist(i)%y1 .or. xy(2)==vlist(i)%y2)) then
            is_inside = .true.
          exit
        end if

        if (xy(2) >  vlist(i)%y2) cycle
        if (xy(2) <= vlist(i)%y1) cycle
        if (xy(1) == vlist(i)%x1) then
          is_inside = .true.
          exit
        end if
        if (xy(1) < vlist(i)%x1) nhits = nhits+1
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
