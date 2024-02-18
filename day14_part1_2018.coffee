
fs = require('fs')
readline = require('readline')

inputStream = fs.createReadStream('input.txt')
rl = readline.createInterface({input: inputStream, terminal: false})

rl.on('line', (line) ->
  input = parseInt(line.trim())
  scoreboard = [3, 7]
  elf1 = 0
  elf2 = 1

  while scoreboard.length < input + 10
    newScore = scoreboard[elf1] + scoreboard[elf2]
    if newScore >= 10
      scoreboard.push(Math.floor(newScore / 10))
    scoreboard.push(newScore % 10)

    elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.length
    elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.length

  console.log(scoreboard.slice(input, input + 10).join(''))
)
