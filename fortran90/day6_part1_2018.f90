
program solve
  implicit none
  integer, parameter :: max_points = 50
  integer :: points_x(max_points), points_y(max_points)
  integer :: num_points, max_x, max_y, i, j, k, dist, min_dist, max_area
  integer, allocatable :: grid(:,:)
  integer, allocatable :: areas(:)
  logical, allocatable :: infinite(:)
  character(len=20) :: line
  integer :: x, y
  
  open(unit=10, file="input.txt", status="old")
  num_points = 0
  max_x = 0
  max_y = 0
  do
    read(10, '(A)', iostat=i) line
    if (i /= 0) exit
    read(line, *, iostat=i) x, y
    if (i /= 0) cycle
    num_points = num_points + 1
    points_x(num_points) = x
    points_y(num_points) = y
    max_x = max(max_x, x)
    max_y = max(max_y, y)
  end do
  close(10)

  allocate(grid(0:max_x+1, 0:max_y+1))
  allocate(areas(num_points))
  allocate(infinite(num_points))
  areas = 0
  infinite = .false.

  do i = 0, max_x + 1
    do j = 0, max_y + 1
      min_dist = max_x + max_y + 1
      grid(i,j) = -1
      do k = 1, num_points
        dist = abs(points_x(k) - i) + abs(points_y(k) - j)
        if (dist < min_dist) then
          min_dist = dist
          grid(i,j) = k
        else if (dist == min_dist) then
          grid(i,j) = -1
        end if
      end do
      if (grid(i,j) /= -1) then
        if (i == 0 .or. j == 0 .or. i == max_x + 1 .or. j == max_y + 1) then
          infinite(grid(i,j)) = .true.
        end if
        areas(grid(i,j)) = areas(grid(i,j)) + 1
      end if
    end do
  end do

  max_area = 0
  do i = 1, num_points
    if (.not. infinite(i) .and. areas(i) > max_area) then
      max_area = areas(i)
    end if
  end do

  print *, max_area

  deallocate(grid)
  deallocate(areas)
  deallocate(infinite)

end program solve
