program solution
  implicit none
  integer :: prev, current, count
  character(len=100) :: line
  integer :: iostat
  integer :: unit

  open(unit, file='input.txt', status='old')
  prev = 0
  count = 0

  do
    read(unit, '(A)', iostat=iostat) line
    if (iostat /= 0) exit
    read(line, *) current
    if (prev /= 0 .and. current > prev) then
      count = count + 1
    end if
    prev = current
  end do

  close(unit)
  print *, count

end program solution