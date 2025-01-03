
program main
  implicit none
  integer :: player1_start, player2_start, player1_pos, player2_pos
  integer :: player1_score, player2_score, die_roll, roll_count, rolls
  character(len=256) :: line
  integer :: iunit, ios

  iunit = 10
  open(unit=iunit, file="input.txt", status="old", action="read", iostat=ios)
  if (ios /= 0) then
    print *, "Error opening input.txt"
    stop
  end if

  read(iunit, '(A)') line
  read(line(29:), *) player1_start
  read(iunit, '(A)') line
  read(line(29:), *) player2_start
  close(iunit)

  player1_pos = player1_start
  player2_pos = player2_start
  player1_score = 0
  player2_score = 0
  die_roll = 1
  roll_count = 0

  do
    rolls = mod(die_roll-1,100) + 1 + mod(die_roll,100) + 1 + mod(die_roll+1,100) + 1
    roll_count = roll_count + 3
    die_roll = die_roll + 3
    player1_pos = mod(player1_pos + rolls - 1, 10) + 1
    player1_score = player1_score + player1_pos
    if (player1_score >= 1000) then
      print *, player2_score * roll_count
      exit
    end if

    rolls = mod(die_roll-1,100) + 1 + mod(die_roll,100) + 1 + mod(die_roll+1,100) + 1
    roll_count = roll_count + 3
    die_roll = die_roll + 3
    player2_pos = mod(player2_pos + rolls - 1, 10) + 1
    player2_score = player2_score + player2_pos
    if (player2_score >= 1000) then
      print *, player1_score * roll_count
      exit
    end if
  end do

end program main
