import strutils, sequtils, streams

proc calculateWaysToWinLongRace(time, record: int): int =
  var waysToWin = 0
  for holdTime in 1..<time:
    let travelTime = time - holdTime
    let distance = holdTime * travelTime
    if distance > record:
      inc waysToWin
  waysToWin

when isMainModule:
  let file = newFileStream("input.txt", fmRead)
  if file == nil:
    echo "Error opening file"
    quit()  # Quit the program if the file cannot be opened

  var time, distance: int
  var lineNumber = 0
  for line in file.lines:
    let parts = line.split(":")
    let value = parts[1].replace(" ", "")
    if lineNumber == 0:
      time = parseInt(value)
    else:
      distance = parseInt(value)
    inc lineNumber

  let waysToWin = calculateWaysToWinLongRace(time, distance)
  echo waysToWin