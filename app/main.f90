program main
  use day2501_mod
  use day2502_mod
  use day2503_mod
  use day2504_mod
  use day2505_mod
  use day2506_mod
  use day2507_mod
  use day2508_mod
  use day2509_mod
  use day2510_mod
  use day2511_mod
! use day2512_mod
  implicit none

  call day2501('inp/01/input.txt')
  call day2502('inp/02/input.txt')
  call day2503('inp/03/input.txt')
  call day2504('inp/04/input.txt')
  call day2505('inp/05/input.txt')
  call day2506('inp/06/input.txt')
  call day2507('inp/07/input.txt')
  call day2508('inp/08/input.txt')
goto 100
 90 call day2509('inp/09/sample.txt')
 stop
 9 call day2509('inp/09/input.txt')
 stop
 100 continue
  call day2510('inp/10/input.txt')
  call day2511('inp/11/input.txt')
end program main
