fs = require 'fs'

findFirstAndLastDigit = (line) ->
  digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  firstDigit = lastDigit = 0

  for char, i in line
    digitStr = char
    if '0' <= digitStr <= '9'
      if firstDigit == 0
        firstDigit = parseInt(digitStr)
      lastDigit = parseInt(digitStr)
    else
      for digit, j in digits
        if line.substr(i).startsWith(digit)
          if firstDigit == 0
            firstDigit = j
          lastDigit = j
          break

  [firstDigit, lastDigit]

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    throw err

  lines = data.split('\n')
  sum = 0

  for line in lines
    [firstDigit, lastDigit] = findFirstAndLastDigit(line)
    sum += 10 * firstDigit + lastDigit

  console.log(sum)