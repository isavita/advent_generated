
program solution
  implicit none
  character :: opponent, yourMove
  integer :: score, totalScore
  character(len=100) :: line
  integer :: iunit, ios

  open(unit=iunit, file='input.txt', status='old', action='read', iostat=ios)
  
  totalScore = 0

  do
    read(iunit, '(A)', iostat=ios) line
    if (ios /= 0) exit

    opponent = line(1:1)
    yourMove = line(3:3)

    score = 0
    if (yourMove == 'X') then
      score = 1
    else if (yourMove == 'Y') then
      score = 2
    else if (yourMove == 'Z') then
      score = 3
    end if

    if ((opponent == 'A' .and. yourMove == 'Y') .or. &
        (opponent == 'B' .and. yourMove == 'Z') .or. &
        (opponent == 'C' .and. yourMove == 'X')) then
      score = score + 6
    else if ((opponent == 'A' .and. yourMove == 'X') .or. &
             (opponent == 'B' .and. yourMove == 'Y') .or. &
             (opponent == 'C' .and. yourMove == 'Z')) then
      score = score + 3
    end if

    totalScore = totalScore + score
  end do

  close(iunit)

  print *, totalScore
end program solution
