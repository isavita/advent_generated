fs = require 'fs'

checkSequence = (scoreboard, sequence) ->
  return false if scoreboard.length < sequence.length
  start = scoreboard.length - sequence.length
  for i, v of sequence
    return false if scoreboard[start + parseInt(i)] isnt v
  true

fs.readFile 'input.txt', 'utf8', (err, data) ->
  scoreboard = [3, 7]
  elf1 = 0
  elf2 = 1
  inputSequence = (parseInt(c) for c in data.trim().split(''))

  loop
    newScore = scoreboard[elf1] + scoreboard[elf2]
    if newScore >= 10
      scoreboard.push Math.floor(newScore / 10)
      break if checkSequence(scoreboard, inputSequence)

    scoreboard.push newScore % 10
    break if checkSequence(scoreboard, inputSequence)

    elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.length
    elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.length

  console.log scoreboard.length - inputSequence.length