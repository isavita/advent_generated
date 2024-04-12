fs = require 'fs'

swapPosition = (password, x, y) ->
  chars = password.split ''
  [chars[x], chars[y]] = [chars[y], chars[x]]  # Corrected multiple assignment
  chars.join ''

swapLetter = (password, x, y) ->
  password.replace(RegExp(x, 'g'), '\0')
          .replace(RegExp(y, 'g'), x)
          .replace(RegExp('\0', 'g'), y)

rotateLeft = (password, steps) ->
  steps %= password.length
  password.slice(steps) + password.slice(0, steps)

rotateRight = (password, steps) ->
  steps %= password.length
  password.slice(-steps) + password.slice(0, -steps)

rotateBasedOnPosition = (password, x) ->
  index = password.indexOf x
  steps = 1 + index
  steps++ if index >= 4
  rotateRight password, steps

reversePositions = (password, x, y) ->
  chars = password.split ''
  while x < y
    [chars[x], chars[y]] = [chars[y], chars[x]]  # Corrected multiple assignment
    x++
    y--
  chars.join ''

movePosition = (password, x, y) ->
  chars = password.split ''
  char = chars.splice(x, 1)[0]
  chars.splice(y, 0, char)
  chars.join ''

applyOperation = (op, password) ->
  fields = op.split ' '
  switch fields[0]
    when 'swap'
      switch fields[1]
        when 'position'
          x = parseInt fields[2]
          y = parseInt fields[5]
          swapPosition password, x, y
        when 'letter'
          x = fields[2]
          y = fields[5]
          swapLetter password, x, y
    when 'rotate'
      switch fields[1]
        when 'left'
          steps = parseInt fields[2]
          rotateLeft password, steps
        when 'right'
          steps = parseInt fields[2]
          rotateRight password, steps
        when 'based'
          x = fields[6]
          rotateBasedOnPosition password, x
    when 'reverse'
      x = parseInt fields[2]
      y = parseInt fields[4]
      reversePositions password, x, y
    when 'move'
      x = parseInt fields[2]
      y = parseInt fields[5]
      movePosition password, x, y

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading input file:", err
    return

  operations = data.trim().split '\n'
  password = 'abcdefgh'

  for op in operations
    password = applyOperation op, password

  console.log password