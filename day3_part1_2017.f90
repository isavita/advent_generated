
program solution
    implicit none
    integer :: target, sideLength, maxValue, stepsFromEdge, distanceToMiddle, i, middlePoint, distance, manhattanDistance
    character(len=100) :: line
    integer :: ios

    open(unit=10, file='input.txt', status='old', action='read')
    read(10, '(A)') line
    read(line, '(I6)') target
    close(10)

    sideLength = ceiling(sqrt(real(target)))
    if (mod(sideLength, 2) == 0) then
        sideLength = sideLength + 1
    end if

    maxValue = sideLength * sideLength
    stepsFromEdge = (sideLength - 1) / 2
    distanceToMiddle = 0

    do i = 0, 3
        middlePoint = maxValue - stepsFromEdge - (sideLength-1)*i
        distance = abs(target - middlePoint)
        if (distance < distanceToMiddle .or. i == 0) then
            distanceToMiddle = distance
        end if
    end do

    manhattanDistance = stepsFromEdge + distanceToMiddle

    print *, manhattanDistance
end program solution
