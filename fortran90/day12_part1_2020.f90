
program main
  implicit none
  integer :: x, y, facing, value, manhattan_distance
  character(len=1) :: action
  character(len=10) :: line
  integer, parameter :: max_line_length = 10
  integer :: i, unit_num
  
  x = 0
  y = 0
  facing = 0
  
  open(newunit=unit_num, file="input.txt", status="old", action="read")
  
  do
    read(unit_num, '(A)', iostat=i) line
    if (i /= 0) exit
    
    action = line(1:1)
    read(line(2:), *) value
    
    select case (action)
      case ('N')
        y = y + value
      case ('S')
        y = y - value
      case ('E')
        x = x + value
      case ('W')
        x = x - value
      case ('L')
        facing = mod(facing - value + 360, 360)
      case ('R')
        facing = mod(facing + value, 360)
      case ('F')
        select case (facing)
          case (0)
            x = x + value
          case (90)
            y = y - value
          case (180)
            x = x - value
          case (270)
            y = y + value
        end select
    end select
  end do
  
  close(unit_num)
  
  manhattan_distance = abs(x) + abs(y)
  print *, manhattan_distance
  
end program main
