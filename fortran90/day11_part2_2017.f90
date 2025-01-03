
program hex_distance
  implicit none
  integer :: x, y, z, max_dist, cur_dist
  character(len=2) :: dir
  character(len=200000) :: line
  integer, allocatable :: directions(:)
  integer :: i, j, n, unit

  open(newunit=unit, file="input.txt", status="old", action="read")
  read(unit, '(A)') line
  close(unit)

  n = 0
  do i = 1, len_trim(line)
    if (line(i:i) == ',') then
      n = n + 1
    end if
  end do
  n = n + 1

  allocate(directions(n))
  j = 1
  n = 1
  do i = 1, len_trim(line)
    if (line(i:i) == ',') then
      read(line(j:i-1),'(A)') dir
      if (dir == "n ") then
        directions(n) = 1
      else if (dir == "ne") then
        directions(n) = 2
      else if (dir == "se") then
        directions(n) = 3
      else if (dir == "s ") then
        directions(n) = 4
      else if (dir == "sw") then
        directions(n) = 5
      else if (dir == "nw") then
        directions(n) = 6
      end if
      j = i + 1
      n = n + 1
    end if
  end do
  read(line(j:),'(A)') dir
  if (dir == "n ") then
    directions(n) = 1
  else if (dir == "ne") then
    directions(n) = 2
  else if (dir == "se") then
    directions(n) = 3
  else if (dir == "s ") then
    directions(n) = 4
  else if (dir == "sw") then
    directions(n) = 5
  else if (dir == "nw") then
    directions(n) = 6
  end if

  x = 0
  y = 0
  z = 0
  max_dist = 0

  do i = 1, size(directions)
    select case (directions(i))
    case (1)
      y = y + 1
      z = z - 1
    case (2)
      x = x + 1
      z = z - 1
    case (3)
      x = x + 1
      y = y - 1
    case (4)
      y = y - 1
      z = z + 1
    case (5)
      x = x - 1
      z = z + 1
    case (6)
      x = x - 1
      y = y + 1
    end select
    cur_dist = (abs(x) + abs(y) + abs(z)) / 2
    max_dist = max(max_dist, cur_dist)
  end do

  print *, max_dist

  deallocate(directions)

end program hex_distance
