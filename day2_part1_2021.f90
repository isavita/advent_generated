program solution
  implicit none
  character(len=10) :: direction
  integer :: units, horizontalPosition = 0, depth = 0, product
  integer :: ios

  open(unit=10, file="input.txt", status="old", action="read")

  do
    read(10, *, iostat=ios) direction, units
    if (ios /= 0) exit

    select case(direction)
      case("forward")
        horizontalPosition = horizontalPosition + units
      case("down")
        depth = depth + units
      case("up")
        depth = depth - units
    end select
  end do

  product = horizontalPosition * depth
  print*, product

  close(10)
end program solution