
program main
  implicit none
  integer, parameter :: max_size = 500
  integer :: target, x, y, dx, dy, value, i, j
  integer :: grid(max_size, max_size)
  character(len=20) :: line
  integer :: ios

  open(unit=10, file="input.txt", status="old", action="read", iostat=ios)
  if (ios /= 0) then
    print *, "Error opening input.txt"
    stop
  end if
  read(10, '(A)', iostat=ios) line
  close(10)
  if (ios /= 0) then
    print *, "Error reading input.txt"
    stop
  end if
  read(line, *) target
  
  grid = 0
  grid(max_size/2+1, max_size/2+1) = 1
  x = 0
  y = 0
  dx = 0
  dy = -1

  do
    if (x == y .or. (x < 0 .and. x == -y) .or. (x > 0 .and. x == 1-y)) then
      i = dx
      dx = -dy
      dy = i
    end if
    x = x + dx
    y = y + dy
    value = 0
    do i = -1, 1
      do j = -1, 1
        value = value + grid(max_size/2+1+x+i, max_size/2+1+y+j)
      end do
    end do
    grid(max_size/2+1+x, max_size/2+1+y) = value
    if (value > target) then
      print *, value
      exit
    end if
  end do
end program main
