program solution
    character :: opponent, roundEnd, yourMove
    integer :: totalScore, score
    character(len=100) :: line

    open(unit=10, file='input.txt', status='old')
    totalScore = 0

    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) then
            exit
        end if

        opponent = line(1:1)
        roundEnd = line(3:3)

        if (roundEnd == 'X') then
            if (opponent == 'A') then
                yourMove = 'Z'
            else if (opponent == 'B') then
                yourMove = 'X'
            else
                yourMove = 'Y'
            end if
        else if (roundEnd == 'Y') then
            if (opponent == 'A') then
                yourMove = 'X'
            else if (opponent == 'B') then
                yourMove = 'Y'
            else
                yourMove = 'Z'
            end if
        else
            if (opponent == 'A') then
                yourMove = 'Y'
            else if (opponent == 'B') then
                yourMove = 'Z'
            else
                yourMove = 'X'
            end if
        end if

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

    close(10)
    print *, totalScore
end program solution