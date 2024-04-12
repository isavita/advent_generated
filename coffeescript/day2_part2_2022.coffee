fs = require 'fs'

calculateScore = (line) ->
  opponent = line[0]
  roundEnd = line[2]

  yourMove = ' '
  if roundEnd == 'X'
    switch opponent
      when 'A' then yourMove = 'Z'
      when 'B' then yourMove = 'X'
      else yourMove = 'Y'
  else if roundEnd == 'Y'
    switch opponent
      when 'A' then yourMove = 'X'
      when 'B' then yourMove = 'Y'
      else yourMove = 'Z'
  else
    switch opponent
      when 'A' then yourMove = 'Y'
      when 'B' then yourMove = 'Z'
      else yourMove = 'X'

  score = 0
  score = switch yourMove
    when 'X' then 1
    when 'Y' then 2
    when 'Z' then 3
    else 0

  if (opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')
    score += 6
  else if (opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z')
    score += 3

  score

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file: #{err}"
    return

  totalScore = 0
  lines = data.split '\n'
  for line in lines when line.trim() isnt ''
    totalScore += calculateScore line

  console.log totalScore