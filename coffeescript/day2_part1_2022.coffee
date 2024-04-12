fs = require 'fs'

calculateScore = (line) ->
  opponent = line[0]
  yourMove = line[2]
  score = switch yourMove
    when 'X' then 1
    when 'Y' then 2
    when 'Z' then 3
    else 0

  if (opponent is 'A' and yourMove is 'Y') or (opponent is 'B' and yourMove is 'Z') or (opponent is 'C' and yourMove is 'X')
    score += 6
  else if (opponent is 'A' and yourMove is 'X') or (opponent is 'B' and yourMove is 'Y') or (opponent is 'C' and yourMove is 'Z')
    score += 3

  score

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  totalScore = 0
  lines = data.trim().split '\n'
  for line in lines
    totalScore += calculateScore line

  console.log totalScore