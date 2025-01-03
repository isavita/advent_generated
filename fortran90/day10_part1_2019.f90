
program asteroid_visibility
  implicit none
  integer, parameter :: max_size = 500
  logical :: asteroids(max_size, max_size)
  integer :: num_rows, num_cols, max_count, i, j
  
  call read_asteroids('input.txt', asteroids, num_rows, num_cols)
  max_count = find_best_asteroid_location(asteroids, num_rows, num_cols)
  print *, max_count
  
contains

  subroutine read_asteroids(filename, asteroids, num_rows, num_cols)
    implicit none
    character(len=*), intent(in) :: filename
    logical, intent(out) :: asteroids(:,:)
    integer, intent(out) :: num_rows, num_cols
    integer :: i, j
    character(len=max_size) :: line
    integer :: ios
    open(unit=10, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      print *, 'Error opening file'
      stop
    end if
    num_rows = 0
    do
      read(10, '(A)', iostat=ios) line
      if (ios /= 0) exit
      num_rows = num_rows + 1
      num_cols = len_trim(line)
      do j = 1, num_cols
        asteroids(num_rows, j) = line(j:j) == '#'
      end do
    end do
    close(10)
  end subroutine read_asteroids

  function find_best_asteroid_location(asteroids, num_rows, num_cols) result(max_count)
    implicit none
    logical, intent(in) :: asteroids(:,:)
    integer, intent(in) :: num_rows, num_cols
    integer :: max_count, count, i, j
    max_count = 0
    do i = 1, num_rows
      do j = 1, num_cols
        if (asteroids(i, j)) then
          count = count_visible_asteroids(asteroids, j, i, num_rows, num_cols)
          max_count = max(max_count, count)
        end if
      end do
    end do
  end function find_best_asteroid_location

  function count_visible_asteroids(asteroids, x, y, num_rows, num_cols) result(count)
    implicit none
    logical, intent(in) :: asteroids(:,:)
    integer, intent(in) :: x, y, num_rows, num_cols
    integer :: count, other_x, other_y, dx, dy, g
    real :: angle
    logical :: seen(0:36000)
    count = 0
    seen = .false.
    do other_y = 1, num_rows
      do other_x = 1, num_cols
        if (asteroids(other_y, other_x) .and. (other_x /= x .or. other_y /= y)) then
          dx = other_x - x
          dy = other_y - y
          g = gcd(dx, dy)
          dx = dx / g
          dy = dy / g
          angle = atan2(real(dy), real(dx))
          angle = angle * 10000.0
          if (angle < 0) angle = angle + 36000.0
          if (.not. seen(int(angle))) then
            count = count + 1
            seen(int(angle)) = .true.
          end if
        end if
      end do
    end do
  end function count_visible_asteroids

  recursive function gcd(a, b) result(res)
    implicit none
    integer, intent(in) :: a, b
    integer :: res
    if (b == 0) then
      res = abs(a)
    else
      res = gcd(b, mod(a, b))
    end if
  end function gcd

end program asteroid_visibility
