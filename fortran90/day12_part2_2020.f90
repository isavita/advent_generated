
program navigate
  implicit none
  integer :: x, y, wx, wy, value, manhattan
  character(len=1) :: action
  character(len=10) :: line
  integer, parameter :: BUF_SIZE = 256
  character(len=BUF_SIZE) :: buffer
  integer :: iostat, unit_num
  
  x = 0
  y = 0
  wx = 10
  wy = 1

  open(newunit=unit_num, file="input.txt", status="old", action="read", iostat=iostat)
  if (iostat /= 0) then
    print *, "Error opening file"
    stop
  end if

  do
    read(unit_num, '(A)', iostat=iostat) buffer
    if (iostat /= 0) exit
    line = trim(buffer)
    action = line(1:1)
    read(line(2:), *) value
    
    select case (action)
      case ('N')
        wy = wy + value
      case ('S')
        wy = wy - value
      case ('E')
        wx = wx + value
      case ('W')
        wx = wx - value
      case ('L')
        call rotate(wx, wy, -value)
      case ('R')
        call rotate(wx, wy, value)
      case ('F')
        x = x + wx * value
        y = y + wy * value
    end select
  end do
  
  close(unit_num)

  manhattan = abs(x) + abs(y)
  print *, manhattan

contains

  subroutine rotate(wx, wy, degrees)
    integer, intent(inout) :: wx, wy
    integer, intent(in) :: degrees
    integer :: temp_wx, temp_wy
    integer :: deg
    
    deg = mod(degrees + 360, 360)
    
    select case (deg)
      case (90)
        temp_wx = wy
        temp_wy = -wx
        wx = temp_wx
        wy = temp_wy
      case (180)
        wx = -wx
        wy = -wy
      case (270)
        temp_wx = -wy
        temp_wy = wx
        wx = temp_wx
        wy = temp_wy
    end select
  end subroutine rotate

  function abs(a)
    integer, intent(in) :: a
    integer :: abs
    if (a < 0) then
      abs = -a
    else
      abs = a
    end if
  end function abs

end program navigate
