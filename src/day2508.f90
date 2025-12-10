  module day2508_mod
    use iso_fortran_env, only : i8 => int64
    use parse_mod, only : string_t, read_strings, split
    use pqueue_mod, only : element_t, pqueue_t
    implicit none

    type :: box_t
      integer(i8) :: x, y, z
      integer :: ind ! circuit id, initialy must be unique for each box
    end type

    type, extends(element_t) :: pair_t
      type(box_t), pointer :: aid, bid
      integer :: ind ! actually not used here
    contains
      procedure :: priority => pair_priority
      procedure :: updateindex => pair_updateindex
      procedure :: element2index => pair_element2index
    end type

  contains

    subroutine day2508(file)
      character(len=*), intent(in) :: file

      type(box_t), allocatable, target :: boxes(:)
      integer :: i, j, ans1, clicks
      integer(i8) :: ans2
      integer, parameter :: MAXHEAPSIZE = 1000*5, PART1_SIZE = 1000
      type(pair_t) :: pair
      type(pair_t), allocatable :: shortest_pairs(:)
      integer, allocatable :: clusters(:)
      class(element_t), allocatable :: oldpair
      type(pqueue_t) :: q

      call read_boxes(file, boxes)

      ! Using MAX HEAP, get a list of shortest pairs
      do i=1, size(boxes)-1
        do j = i+1, size(boxes)
          pair%aid => boxes(i)
          pair%bid => boxes(j)

          if (q%size() < MAXHEAPSIZE) then
            ! add every pair until heap is full
            call q%insert(pair)
          else
            ! if shorter pair found, replace the longest pair (top) in heap
            associate(peek=>q%peek())
              if (peek%priority() > pair%priority()) then
                call q%top(oldpair)
                call q%insert(pair)
              end if
            end associate
          end if
        end do
      end do

      ! copy pairs from the heap to "shortest_pairs"
      allocate(shortest_pairs(q%size()))
      do i=q%size(), 1, -1
        call q%top(oldpair)
        select type(oldpair)
        class is (pair_t)
          shortest_pairs(i) = oldpair
        end select
      end do
      if (q%size()/=0) error stop 'size not zero'

      ! connect boxes using "shortest_pairs" array (PART 1)
      clicks = 0
      do i=1, PART1_SIZE
        block
          integer :: oldind, newind
          oldind = max(shortest_pairs(i)%aid%ind, shortest_pairs(i)%bid%ind)
          newind = min(shortest_pairs(i)%aid%ind, shortest_pairs(i)%bid%ind)
          if (oldind /= newind) then
            clicks = clicks + 1
            where (boxes%ind == oldind)
              boxes%ind = newind
            end where
          end if
        end block
      end do

      ! count connected clusters and select three largest clusters
      allocate(clusters(size(boxes)))
      do i=1,size(boxes)
        clusters(i) = count(boxes%ind==i)
      end do
      ans1 = 1
      do i=1, 3
        j = maxloc(clusters,1)
        ans1 = ans1 * clusters(j)
        clusters(j) = -clusters(j)
      end do

      ! continue connecting clusters until only single cluster remains
      do i=PART1_SIZE+1, size(shortest_pairs)
        block
          integer :: oldind, newind
          oldind = max(shortest_pairs(i)%aid%ind, shortest_pairs(i)%bid%ind)
          newind = min(shortest_pairs(i)%aid%ind, shortest_pairs(i)%bid%ind)
          if (oldind /= newind) then
            clicks = clicks + 1
            where (boxes%ind == oldind)
              boxes%ind = newind
            end where
          end if
          if (clicks == size(boxes)-1) exit
        end block
      end do
      ans2 = shortest_pairs(i)%aid%x * shortest_pairs(i)%bid%x

      ! report
      print '("08/1: ",i0,l2)', ans1, ans1==112230
      print '("08/1: ",i0,l2)', ans2, ans2==2573952864_i8
    end subroutine day2508


    subroutine read_boxes(file, boxes)
      character(len=*), intent(in) :: file
      type(box_t), allocatable, intent(inout) :: boxes(:)

      type(string_t), allocatable :: lines(:), words(:)
      integer :: i

      lines = read_strings(file)
      allocate(boxes(size(lines)))
      do i=1, size(boxes)
        call split(lines(i)%str,',',words)
        if (size(words)/=3) error stop 'three numbers expected'
        read(words(1)%str,*) boxes(i)%x
        read(words(2)%str,*) boxes(i)%y
        read(words(3)%str,*) boxes(i)%z
        boxes(i)%ind = i
      end do
    end subroutine read_boxes


    integer(i8) function pair_priority(this) result(p)
      class(pair_t), intent(in) :: this

      p = (this%aid%x - this%bid%x)**2 + &
          (this%aid%y - this%bid%y)**2 + &
          (this%aid%z - this%bid%z)**2
    end function pair_priority


    subroutine pair_updateindex(this, index)
      class(pair_t), intent(inout) :: this
      integer, intent(in) :: index
      this%ind = index
    end subroutine pair_updateindex


    integer function pair_element2index(this) result(ind)
      class(pair_t), intent(in) :: this
      ind = this%ind
    end function pair_element2index

  end module day2508_mod
