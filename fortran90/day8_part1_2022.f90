
program visible_trees
  implicit none
  integer, parameter :: max_size = 100
  integer :: grid(max_size, max_size), visible(max_size, max_size)
  integer :: rows, cols, i, j, x, y, dx, dy, nx, ny
  character(len=max_size) :: line
  integer, allocatable :: row_lengths(:)
  
  open(unit=10, file="input.txt", status="old", action="read")
  rows = 0
  cols = 0
  do
    read(10, '(A)', iostat=i) line
    if (i /= 0) exit
    rows = rows + 1
    cols = len_trim(line)
    do j = 1, cols
      grid(rows, j) = ichar(line(j:j)) - ichar('0')
    end do
  end do
  close(10)

  allocate(row_lengths(rows))
  do i = 1, rows
    row_lengths(i) = cols
  end do

  visible = 0
  do y = 1, rows
    do x = 1, row_lengths(y)
      do dx = -1, 1
        do dy = -1, 1
          if (abs(dx) + abs(dy) /= 1) cycle
          nx = x
          ny = y
          do
            nx = nx + dx
            ny = ny + dy
            if (nx < 1 .or. nx > row_lengths(y) .or. ny < 1 .or. ny > rows) then
              visible(y, x) = 1
              exit
            end if
            if (grid(ny, nx) >= grid(y, x)) exit
          end do
        end do
      end do
    end do
  end do

  print *, sum(visible)
  deallocate(row_lengths)
end program visible_trees
