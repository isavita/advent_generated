
program cube_game
  implicit none
  integer, parameter :: max_line_length = 1000
  character(len=max_line_length) :: line
  integer :: game_id, total_power, i, j, k, start, finish, count, red, green, blue, max_red, max_green, max_blue, power
  character(len=10) :: color
  integer, dimension(:), allocatable :: counts
  character(len=:), allocatable :: round
  
  open(unit=10, file="input.txt", status="old", action="read")
  total_power = 0
  
  do
    read(10, '(A)', iostat=i) line
    if (i /= 0) exit
    
    start = index(line, "Game ") + 5
    finish = index(line, ":") - 1
    read(line(start:finish), *) game_id
    
    start = finish + 3
    
    max_red = 0
    max_green = 0
    max_blue = 0
    
    do
      finish = index(line(start:), ";")
      if (finish == 0) then
        round = line(start:)
      else
        round = line(start:start+finish-2)
      end if
      
      red = 0
      green = 0
      blue = 0
      
      j = 1
      do
        k = index(round(j:), " ")
        if (k == 0) exit
        read(round(j:j+k-2), *) count
        j = j + k
        k = index(round(j:), ",")
        if (k == 0) then
          color = round(j:)
        else
          color = round(j:j+k-2)
        end if
        
        if (color == "red") then
          red = red + count
        else if (color == "green") then
          green = green + count
        else if (color == "blue") then
          blue = blue + count
        end if
        
        if (k == 0) exit
        j = j + k + 1
      end do
      
      max_red = max(max_red, red)
      max_green = max(max_green, green)
      max_blue = max(max_blue, blue)
      
      if (finish == 0) exit
      start = start + finish + 1
    end do
    
    power = max_red * max_green * max_blue
    total_power = total_power + power
  end do
  
  close(10)
  print *, total_power
end program cube_game
