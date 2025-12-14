  module day2509_mod
    use iso_fortran_env, only : i8 => int64
    use parse_mod, only : string_t, read_strings

    type edge_t
      ! x1 < x2, y1 < y2
      integer(i8) :: x1, x2, y1, y2
      logical :: vertical
    contains
      procedure :: isvalid => edge_isvalid
    end type edge_t

    type box_t
      integer(i8) :: x1, x2, y1, y2
    contains
      procedure :: area => box_area, inside => box_inside
    end type box_t
    interface box_t
      module procedure box_new
    end interface

  contains

    subroutine day2509(file)
      character(len=*), intent(in) :: file

      integer(i8), allocatable :: xy(:,:)
      integer(i8) :: ans1, ans2, a
      integer :: i, j
      type(edge_t), allocatable :: polygon(:)
      type(box_t) :: box

      call read_xy(file, xy)
      polygon = edge_list(xy)
      ans1 = 0
      ans2 = 0
      do i=1, size(xy,2)-1
        do j=i+1, size(xy,2)
          box = box_t(xy(:,i), xy(:,j))
          ! ignore (1 x n) or (m x 1) boxes as they are unlikely to be
          ! the maximum area and create edge cases
          if (box%x1==box%x2 .or. box%y1==box%y2) cycle

          ! Part 1 - no topological constrains
          a = box%area()
          if (a > ans1) ans1 = a

          ! Part 2 - rectangle must be fully inside polygon
          if (a > ans2) then
            if (box%inside(polygon)) ans2 = a
          end if
        end do
      end do
      print '("09/1: ",i0,l2)', ans1, ans1==4745816424_i8
      print '("09/1: ",i0,l2)', ans2, ans2==1351617690_i8 ! too high
    end subroutine day2509


    pure function box_inside(this, edges) result(is)
      class(box_t), intent(in) :: this
      type(edge_t), intent(in) :: edges(:)
      logical :: is
!
! Part 2 logic - rectangle is ok if: 
! (1) it does not cross any polygon edge
! (2) one of its points is inside the polygon
!
! It seems the condition (2) is not needed for the problem input
!
      type(edge_t) :: rectangle(4)
      logical :: found_crossing_edge, point_inside
      integer(i8) :: p(2)

      rectangle = box_edges(this)
      found_crossing_edge = is_anyedge_crossing(edges, rectangle)

      p(1) = 2*this%x1 + 1
      p(2) = 2*this%y1 + 1
      point_inside = is_point_inside(p, edges)

     is = (.not. found_crossing_edge) .and. point_inside
     !is = (.not. found_crossing_edge) ! this also works
    end function box_inside


    pure logical function is_anyedge_crossing(aa, bb) result(is)
      type(edge_t), intent(in) :: aa(:), bb(:)
!
! Edge crossing detection.
!
      integer :: i, j
      type(edge_t) :: hor, ver

      ! assuming there are no crossings, until we found one
      is = .false.

      LOOP: do i=1, size(aa)
        do j=1, size(bb)
          ! ignore parallel edges
          if (aa(i)%vertical .eqv. bb(j)%vertical) cycle

          if (aa(i)%vertical) then
            ver = aa(i)
            hor = bb(j)
          else
            hor = aa(i)
            ver = bb(j)
          end if
          ! horizontal line is above or under vertical edge
          if (ver%y2 < hor%y1 .or. ver%y1 > hor%y1 ) cycle
          ! vertical line is left or right of horizontal edge
          if (hor%x2 < ver%x1 .or. hor%x1 > ver%x1 ) cycle
          ! we found crossing edges
          is = .true.
          exit LOOP
        end do
      end do LOOP
    end function is_anyedge_crossing


    pure logical function is_point_inside(p, edges)
      integer(i8), intent(in) :: p(2)
      type(edge_t), intent(in) :: edges(:)
!
! From point (x,y), make ray eastwards and count the number of intersecting
! edges. Assuming that (x,y) do not coincide with any of (x1,y1)-(x2,y2)
! edge co-ordinates. 
!
      integer :: nhits, i

      nhits = 0
      do i=1, size(edges)
        if (.not. edges(i)%vertical) then
          ! horizontal edge can be ignored, assert it do not pass through (x,y)
          if (edges(i)%y1==p(2)) error stop 'ray can not go along horizontal edge'
        else if (edges(i)%x1 > p(1)) then
          ! vertical edge east of (x,y)
          if (p(2)==edges(i)%y1 .or. p(2)==edges(i)%y2) &
            error stop 'ray touching edge end points'
          if (p(2)<edges(i)%y2 .and. p(2)>edges(i)%y1) nhits = nhits+1
        else if (edges(i)%x1 < p(1)) then
          ! vertical edge west of (x,y)
          continue
        else
          ! vertical edge that may pass through (x,y)
          error stop 'vertical edge at same coordinate found'
        end if
      end do

      ! point is inside, if ray crossed odd number of times
      is_point_inside = modulo(nhits,2)==1
    end function is_point_inside


    pure function box_new(p1, p2) result(new)
      integer(i8), intent(in) :: p1(2), p2(2)
      type(box_t) :: new
!
! New rectangle out of points (x1,y1) and (x2,y2)
!
      new%x1 = min(p1(1), p2(1))
      new%y1 = min(p1(2), p2(2))
      new%x2 = max(p1(1), p2(1))
      new%y2 = max(p1(2), p2(2))
    end function box_new


    pure function box_area(this) result(a)
      class(box_t), intent(in) :: this
      integer(i8) :: a
      a = (this%x2-this%x1+1) * (this%y2-this%y1+1)
    end function box_area


    pure function box_edges(this) result(edges)
      class(box_t), intent(in) :: this
      type(edge_t) :: edges(4)
! 
! Make edge list of the rectangle. Coordinates of edge-end points are
! multiplied by two and then moved one unit towards rectangle interior, 
! therefore they are all ODD.
!
      integer(i8) :: ax1, ax2, ay1, ay2

      ax1 = 2*this%x1 + 1
      ax2 = 2*this%x2 - 1
      ay1 = 2*this%y1 + 1
      ay2 = 2*this%y2 - 1
      edges(1) = edge_t(x1=ax1, x2=ax1, y1=ay1, y2=ay2, vertical=.true.)
      edges(2) = edge_t(x1=ax2, x2=ax2, y1=ay1, y2=ay2, vertical=.true.)
      edges(3) = edge_t(x1=ax1, x2=ax2, y1=ay1, y2=ay1, vertical=.false.)
      edges(4) = edge_t(x1=ax1, x2=ax2, y1=ay2, y2=ay2, vertical=.false.)
      if (.not. all(edges%isvalid())) error stop 'rectangles edges are not valid'
    end function box_edges


    pure function edge_list(xy) result(edges)
      integer(i8), intent(in) :: xy(:,:)
      type(edge_t), allocatable :: edges(:)
!
! Points from "xy" form a closed polygon, make edge list of this polygon.
! Coordinates of edge end-points are multiplied by two and  therefore all 
! are EVEN.
!
      integer :: i, j

      allocate(edges(size(xy,2)))
      do i=1, size(xy,2)
        j = i+1
        if (j>size(xy,2)) j =1
        edges(i)%x1 = 2*min(xy(1,i),xy(1,j))
        edges(i)%x2 = 2*max(xy(1,i),xy(1,j))
        edges(i)%y1 = 2*min(xy(2,i),xy(2,j))
        edges(i)%y2 = 2*max(xy(2,i),xy(2,j))
        edges(i)%vertical = edges(i)%x1 == edges(i)%x2
        if (.not. edges(i)%isvalid()) error stop 'edge is not valid'
      end do
    end function edge_list


    pure elemental logical function edge_isvalid(this) result(is)
      class(edge_t), intent(in) :: this
!
! Assert, that following is true for the edge:
! x1 <= x2, y1 <= y2
!
      is = this%x1 <= this%x2 .and. this%y1 <= this%y2
    end function edge_isvalid


    subroutine read_xy(file, xy)
      character(len=*), intent(in) :: file
      integer(i8), allocatable, intent(out) :: xy(:,:)
!
! Reading points from the file into rank-2 array "xy"
!
!  xy = [ x1 x2 x3 ...
!         y1 y2 y3 ... ]
!
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
