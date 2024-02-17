program day3
  implicit none
  integer :: a, b, c, count
  open(unit=10, file='input.txt', status='old')
  count = 0
  do while(.true.)
    read(10, *, iostat=a) a, b, c
    if (a < 0) exit
    if (a + b > c .and. a + c > b .and. b + c > a) then
      count = count + 1
    end if
  end do
  close(10)
  print*, count
end program day3