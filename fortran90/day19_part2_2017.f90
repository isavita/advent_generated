
program main
  implicit none
  character(len=1000), allocatable :: grid(:)
  integer :: i, j, x, y, dx, dy, steps, nlines, ncol
  character(len=1) :: cell
  
  open(unit=10, file="input.txt", status="old", action="read")
  nlines = 0
  do
    read(10, *, iostat=i)
    if (i /= 0) exit
    nlines = nlines + 1
  end do
  rewind(10)
  allocate(grid(nlines))
  do i = 1, nlines
    read(10, '(A)') grid(i)
  end do
  close(10)

  ncol = len(grid(1))
  do i = 1, ncol
    if (grid(1)(i:i) == '|') then
      x = i
      exit
    end if
  end do

  y = 1
  dx = 0
  dy = 1
  steps = 0

  do
    if (x < 1 .or. x > ncol .or. y < 1 .or. y > nlines) exit
    cell = grid(y)(x:x)
    if (cell == ' ') exit
    steps = steps + 1
    if (cell == '+') then
      if (dx == 0) then
        if (x > 1 .and. (grid(y)(x-1:x-1) == '-' .or. (grid(y)(x-1:x-1) >= 'A' .and. grid(y)(x-1:x-1) <= 'Z'))) then
          dx = -1
          dy = 0
        else
          dx = 1
          dy = 0
        end if
      else
        if (y > 1 .and. (grid(y-1)(x:x) == '|' .or. (grid(y-1)(x:x) >= 'A' .and. grid(y-1)(x:x) <= 'Z'))) then
          dx = 0
          dy = -1
        else
          dx = 0
          dy = 1
        end if
      end if
    end if
    x = x + dx
    y = y + dy
  end do

  print *, steps
  deallocate(grid)
end program main
