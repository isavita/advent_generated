
program maze
  implicit none
  integer :: favoriteNumber, steps, start_x, start_y, target_x, target_y
  integer, allocatable :: queue_x(:), queue_y(:), visited(:,:)
  integer :: head, tail, current_x, current_y, next_x, next_y, i, j, size
  integer, parameter :: max_size = 1000
  
  open(unit=10, file="input.txt", status="old", action="read")
  read(10,*) favoriteNumber
  close(10)

  start_x = 1
  start_y = 1
  target_x = 31
  target_y = 39

  allocate(queue_x(max_size), queue_y(max_size), visited(200,200))
  visited = 0
  queue_x(1) = start_x
  queue_y(1) = start_y
  visited(start_x, start_y) = 1
  head = 1
  tail = 1
  steps = 0

  do while (head <= tail)
    size = tail - head + 1
    do i = 1, size
      current_x = queue_x(head)
      current_y = queue_y(head)
      head = head + 1
      if (current_x == target_x .and. current_y == target_y) then
        print *, steps
        deallocate(queue_x, queue_y, visited)
        stop
      end if
      do j = -1, 1, 2
        next_x = current_x + j
        if (next_x >= 0 .and. next_x < 200) then
          if (.not. is_wall(favoriteNumber, next_x, current_y) .and. visited(next_x, current_y) == 0) then
            tail = tail + 1
            queue_x(tail) = next_x
            queue_y(tail) = current_y
            visited(next_x, current_y) = 1
          end if
        end if
        next_y = current_y + j
        if (next_y >= 0 .and. next_y < 200) then
          if (.not. is_wall(favoriteNumber, current_x, next_y) .and. visited(current_x, next_y) == 0) then
            tail = tail + 1
            queue_x(tail) = current_x
            queue_y(tail) = next_y
            visited(current_x, next_y) = 1
          end if
        end if
      end do
    end do
    steps = steps + 1
  end do
contains
  function is_wall(favoriteNumber, x, y) result(iswall)
    integer, intent(in) :: favoriteNumber, x, y
    logical :: iswall
    integer :: num, bits
    num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
    bits = 0
    do while (num > 0)
      if (mod(num, 2) == 1) bits = bits + 1
      num = num / 2
    end do
    iswall = mod(bits, 2) /= 0
  end function is_wall
end program maze
