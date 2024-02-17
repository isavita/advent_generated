program solution
  character(8) :: direction
  integer :: units, horizontalPosition, depth, aim, product
  open(10, file='input.txt')

  horizontalPosition = 0
  depth = 0
  aim = 0

  do
    read(10, *, iostat=ios) direction, units
    if (ios /= 0) exit
    select case(direction)
    case('forward')
      horizontalPosition = horizontalPosition + units
      depth = depth + aim * units
    case('down')
      aim = aim + units
    case('up')
      aim = aim - units
    end select
  end do

  product = horizontalPosition * depth
  print *, product
end program solution