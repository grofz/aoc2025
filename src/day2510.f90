module day2510_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : i8 => int64
  implicit none

  type machine_t
    integer :: nbits
    integer :: target ! display state
    integer, allocatable :: buttons(:)
    integer, allocatable :: joltage(:)
    integer, allocatable :: s(:)
  end type

contains

  subroutine day2510(file)
    character(len=*), intent(in) :: file

    type(machine_t), allocatable :: machines(:)
    integer :: i, fid
    integer(i8) :: ans1

    machines = read_machines(file)
    ans1 = 0

    ! generate matlab code for Part 2
    open(newunit=fid, file='day10.m', status='replace')
    write(fid,'("ans2 = 0;")')
    write(fid,'("A=[];")')
    write(fid,'("b=[];")')

    do i=1, size(machines)
      ans1 = ans1 + find_min_presses(machines(i))
      call solve_machine_matlab(fid, machines(i))
      write(fid,'("ans2 = ans2 + fval;")')
    end do

    close(fid)
    print '("10/1: ",i0,l2)', ans1, ans1==488
    print '("10/2: ? (run matlab)")'

  end subroutine day2510


  function find_min_presses(m) result(ans)
    type(machine_t), intent(inout) :: m
    integer :: ans
!
! Solver for Part 1 (BFS)
! Actualy it is trivial as each button should not be pressed more than once
!
    integer :: i, k, next, path_len

    path_len = 0
    MAIN: do
      do i=lbound(m%s,1), ubound(m%s,1)
        if (m%s(i)/=path_len) cycle
        ! found state with current path length
        do k=1, size(m%buttons)
          ! exclusive or to "swap" connected lights
          next = ieor(i, m%buttons(k))
          ! save next state if not yet touched
          if (m%s(next) > path_len+1) m%s(next) = path_len+1
          if (next == m%target) then
            ans = path_len+1
            exit MAIN
          end if
        end do
      end do
      ! increase path_len
      path_len = path_len + 1
    end do MAIN
  end function

! ====================
! Generate Matlab Code
! ====================
! For Part 2 using "intlinprog" function in Matlab
! TODO write own solver :-)

  subroutine solve_machine_matlab(fid, m)
    type(machine_t), intent(in) :: m
    integer, intent(in) :: fid

    integer :: A(size(m%joltage), size(m%buttons)), b(size(m%joltage))
    integer :: i, j
    character(len=size(m%joltage)) :: bits

    if (m%nbits /= size(m%joltage)) error stop 'solve_machine'

    ! make matrices Aeq and beq
    do i=1, size(b)
      b(i) = m%joltage(i)
    end do
    A = 0
    do j=1, size(A,2)
      bits = num2str(m%buttons(j), m%nbits)
      do i=1, len(bits)
        if (bits(i:i)=='#') A(i,j)=1
      end do
    end do

    write(fid,'("Aeq = [ ...")')
    do i=1,size(A,1)
      write(fid,'(*(i2,1x))',advance='no') A(i,:)
      write(fid,'("; ...")')
    end do
    write(fid,'("];")')

    write(fid,'("beq = [ ")',advance='no')
    write(fid,'(*(i0,1x))',advance='no') b(:)
    write(fid,'("]'';")')

    write(fid,'("f = ones(",i0,",1);")') size(A,2)
    write(fid,'("lb = zeros(",i0,",1);")') size(A,2)
    write(fid,'("ub = Inf*ones(",i0,",1);")') size(A,2)
    write(fid,'("intcom = 1:",i0,";")') size(A,2)
    write(fid,'("[x,fval]=intlinprog(f,intcom,A,b,Aeq,beq,lb,ub)")')
    write(fid,*)
  end subroutine

! =============
! Parsing chore
! =============

  function read_machines(file) result(machines)
    character(len=*), intent(in) :: file
    type(machine_t), allocatable :: machines(:)

    type(string_t), allocatable :: lines(:)
    integer :: i

    lines = read_strings(file)
    allocate(machines(size(lines)))
    do i=1, size(machines)
      machines(i) = machine_from_str(lines(i)%str)
      allocate(machines(i)%s(0: 2**(machines(i)%nbits)-1), source=huge(machines(i)%s)) 
      machines(i)%s(0) = 0 ! initial state (reached in 0 steps)
    end do
  end function


  subroutine display_machine(this)
    class(machine_t), intent(in) :: this
    integer :: i
    print '("Target = ",a,1x,i0)', num2str(this%target, this%nbits), this%target
    print '(*(a,3x))', (num2str(this%buttons(i), this%nbits), i=1,size(this%buttons))
    print '("Joltage = ",*(i0,1x))', this%joltage
    print *
  end subroutine


  function machine_from_str(str) result(new)
    character(len=*), intent(in) :: str
    type(machine_t) :: new

    integer, dimension(2) :: pos_but, pos_jol, pos_dis
    type(string_t), allocatable :: toks(:)
    integer :: i

    pos_dis(1) = index(str,'[')
    pos_dis(2) = index(str,']', back=.true.)
    pos_but(1) = index(str,'(')
    pos_but(2) = index(str,')', back=.true.)
    pos_jol(1) = index(str,'{')
    pos_jol(2) = index(str,'}', back=.true.)
    if (pos_dis(1)==0 .or. pos_but(1)==0 .or. pos_jol(1)==0 .or. any(pos_but < pos_dis) &
     .or.  any(pos_jol < pos_but)) error stop 'could not parse line'

    B1: block
      call split(str(pos_dis(1):pos_dis(2)), ' ', toks)
      if (size(toks)/=1) error stop 'expecting one []'
      new%nbits = len(strip(toks(1)%str,'[]'))
      new%target = str2num(strip(toks(1)%str,'[]'))
    end block B1

    B2: block
      call split(str(pos_but(1):pos_but(2)), ' ', toks)
      allocate(new%buttons(size(toks)))
      do i=1, size(toks)
        new%buttons(i) = dig2num(strip(toks(i)%str,'()'), new%nbits)
      end do
    end block B2

    B3: block
      type(string_t), allocatable :: words(:)
      call split(str(pos_jol(1):pos_jol(2)), ' ', toks)
      if (size(toks)/=1) error stop 'expecting one {}'
      call split(strip(toks(1)%str,'{}'), ',', words)
      allocate(new%joltage(size(words)))
      do i=1, size(words)
        read(words(i)%str,*) new%joltage(i)
      end do
    end block B3
  end function


  function dig2num(str, nbits) result(num)
    character(len=*), intent(in) :: str
    integer, intent(in) :: nbits
    integer :: num

    character(len=nbits) :: bits
    type(string_t), allocatable :: digits(:)
    integer :: i, j

    bits = repeat('.', len(bits))
    call split(str,',',digits)
    do i=1, size(digits)
      read(digits(i)%str,*) j
      bits(j+1:j+1) = '#'
    end do
    num = str2num(bits)
  end function


  pure function str2num(str) result(num)
    character(len=*), intent(in) :: str
    integer :: num

    integer :: i, j
    num = 0
    do i=len(str),1,-1
      j = len(str)-i
      select case(str(i:i))
      case('#')
        num = num + 2**j
      case('.')
        continue
      case default
        error stop 'unexpectec char in str2num'
      end select
    end do
  end function


  pure function num2str(num, nbits) result(str)
    integer, intent(in) :: num, nbits
    character(len=nbits) :: str

    integer :: i, rem, div

    rem = num
    do i=1, nbits
      div = rem/(2**(nbits-i))
      if (div==1) then
        str(i:i) = '#'
      else if (div==0) then
        str(i:i) = '.'
      else 
        error stop 'num2str logical error'
      end if
      rem = rem - div*2**(nbits-i)
    end do
  end function


  pure function strip(str,chars) result(stripped)
    character(len=*), intent(in) :: str
    character(len=2), intent(in) :: chars
    character(len=len(str)-2) :: stripped

    if (str(:1)/=chars(1:1) .or. str(len(str):)/=chars(2:2)) error stop 'strip error'
    stripped = str(2:len(str)-1)
  end function

end module day2510_mod