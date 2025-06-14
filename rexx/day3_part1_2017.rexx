
call main
exit

main:
  target = linein('input.txt')

  sideLength = 1
  do while sideLength * sideLength < target
    sideLength = sideLength + 2
  end

  stepsFromEdge = (sideLength - 1) % 2
  maxValue = sideLength * sideLength

  distanceToMiddle = abs(target - (maxValue - stepsFromEdge))
  do i = 1 to 3
    middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i
    distance = abs(target - middlePoint)
    if distance < distanceToMiddle then
      distanceToMiddle = distance
  end

  manhattanDistance = stepsFromEdge + distanceToMiddle
  say manhattanDistance
return
