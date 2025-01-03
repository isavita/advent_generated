
program solve
  implicit none
  integer, parameter :: max_size = 1000
  character(len=max_size) :: grid(max_size)
  integer :: h, w, x, y, dir_x, dir_y, dir_idx, i, j, nx, ny
  logical :: visited(max_size, max_size)
  integer :: dirs(4, 2)
  logical :: found
  integer :: file_status
  integer :: count

  open(unit=10, file='input.txt', status='old', iostat=file_status)
  if (file_status /= 0) then
    print *, 'Error opening file'
    stop
  end if

  h = 0
  do
    read(10, '(A)', iostat=file_status) grid(h + 1)
    if (file_status /= 0) exit
    h = h + 1
  end do
  close(10)

  w = len_trim(grid(1))
  dirs = reshape((/0, 1, 0, -1, -1, 0, 1, 0/), (/4, 2/))
  found = .false.
  do i = 1, h
    do j = 1, w
      select case (grid(i)(j:j))
        case ('^')
          x = j
          y = i
          dir_idx = 1
          dir_x = dirs(dir_idx, 1)
          dir_y = dirs(dir_idx, 2)
          found = .true.
        case ('>')
          x = j
          y = i
          dir_idx = 2
          dir_x = dirs(dir_idx, 1)
          dir_y = dirs(dir_idx, 2)
          found = .true.
        case ('v')
          x = j
          y = i
          dir_idx = 3
          dir_x = dirs(dir_idx, 1)
          dir_y = dirs(dir_idx, 2)
          found = .true.
        case ('<')
          x = j
          y = i
          dir_idx = 4
          dir_x = dirs(dir_idx, 1)
          dir_y = dirs(dir_idx, 2)
          found = .true.
      end select
      if (found) exit
    end do
    if (found) exit
  end do

  visited = .false.
  visited(y, x) = .true.
  count = 1

  do
    nx = x + dir_x
    ny = y + dir_y
    if (nx < 1 .or. nx > w .or. ny < 1 .or. ny > h) exit
    if (grid(ny)(nx:nx) == '#') then
      dir_idx = mod(dir_idx, 4) + 1
      dir_x = dirs(dir_idx, 1)
      dir_y = dirs(dir_idx, 2)
      cycle
    end if
    x = nx
    y = ny
    if (.not. visited(y, x)) then
        visited(y, x) = .true.
        count = count + 1
    end if
  end do

  print *, count
end program solve
