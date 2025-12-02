module day2501_mod
  use parse_mod, only : read_strings, string_t
  implicit none
  private
  public day2501

contains

  subroutine day2501(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: pos, i, j, s, x, ans1, ans2
    integer, parameter :: n=100

    lines = read_strings(file)
    pos = 50
    ans1 = 0
    ans2 = 0
    do i=1, size(lines)
      select case(lines(i)%str(1:1))
      case('L')
        s = -1
      case('R')
        s = 1
      case default
        error stop 'invalid input'
      end select
      read(lines(i)%str(2:),*) x

      do j=1,x
        pos = pos + s
        if (pos==-1) pos = 99
        if (pos==n) pos = 0
        if (pos==0) ans2 = ans2 + 1
      end do
      if (pos==0) ans1 = ans1 + 1
    end do

    print '("01/1: ",i0,1x,l)', ans1, ans1==1059
    print '("01/2: ",i0,1x,l)', ans2, ans2==6305
  end subroutine day2501

end module day2501_mod
