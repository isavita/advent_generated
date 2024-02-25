
proc main() =
  var file = open("input.txt")
  if file == nil:
    echo "Error opening file:"
    quit(1)

  var line: string
  var totalScore = 0

  while not file.endOfFile:
    line = file.readLine()
    var opponent = line[0]
    var roundEnd = line[2]

    var yourMove: char

    if roundEnd == 'X':
      if opponent == 'A':
        yourMove = 'Z'
      elif opponent == 'B':
        yourMove = 'X'
      else:
        yourMove = 'Y'
    elif roundEnd == 'Y':
      if opponent == 'A':
        yourMove = 'X'
      elif opponent == 'B':
        yourMove = 'Y'
      else:
        yourMove = 'Z'
    else:
      if opponent == 'A':
        yourMove = 'Y'
      elif opponent == 'B':
        yourMove = 'Z'
      else:
        yourMove = 'X'

    var score = 0

    if yourMove == 'X':
      score = 1
    elif yourMove == 'Y':
      score = 2
    elif yourMove == 'Z':
      score = 3

    if (opponent == 'A' and yourMove == 'Y') or (opponent == 'B' and yourMove == 'Z') or (opponent == 'C' and yourMove == 'X'):
      score += 6
    elif (opponent == 'A' and yourMove == 'X') or (opponent == 'B' and yourMove == 'Y') or (opponent == 'C' and yourMove == 'Z'):
      score += 3

    totalScore += score

  file.close()

  echo totalScore

main()
