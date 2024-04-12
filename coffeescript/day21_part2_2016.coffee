fs = require 'fs'

class Scrambler
  constructor: (pw) ->
    @pw = pw.split ''

  toString: ->
    @pw.join ''

  swapPositions: (x, y) ->
    [@pw[x], @pw[y]] = [@pw[y], @pw[x]]

  swapLetters: (x, y) ->
    xIndex = @pw.indexOf x
    yIndex = @pw.indexOf y
    @swapPositions xIndex, yIndex

  rotate: (steps) ->
    length = @pw.length
    steps %= length
    steps += length if steps < 0
    @pw = @pw.slice(-steps).concat @pw.slice(0, -steps)

  rotateLetter: (x) ->
    index = @pw.indexOf x
    index++ if index >= 4
    @rotate index + 1

  derotateLetter: (x) ->
    index = @pw.indexOf x
    if index % 2 == 1
      rot = -Math.floor((index + 1) / 2)
    else if index != 0
      rot = Math.floor((6 - index) / 2)
    else
      rot = -1
    @rotate rot

  reverse: (x, y) ->
    while x < y
      [@pw[x], @pw[y]] = [@pw[y], @pw[x]]
      x++
      y--

  move: (x, y) ->
    ch = @pw[x]
    if x < y
      @pw.splice x, 1
      @pw.splice y, 0, ch
    else
      @pw.splice x, 1
      @pw.splice y, 0, ch

  scramble: (instructions, direction) ->
    instructions.reverse() if direction < 0
    for instruction in instructions
      line = instruction.split ' '
      switch line[0]
        when 'swap'
          x = line[2]
          y = line[5]
          if line[1] == 'position'
            @swapPositions parseInt(x), parseInt(y)
          else
            @swapLetters x[0], y[0]
        when 'rotate'
          if line[1] == 'based'
            if direction > 0
              @rotateLetter line[6][0]
            else
              @derotateLetter line[6][0]
          else
            steps = parseInt line[2]
            steps = -steps if line[1] == 'left'
            steps = -steps if direction < 0
            @rotate steps
        when 'reverse'
          x = parseInt line[2]
          y = parseInt line[4]
          @reverse x, y
        when 'move'
          x = parseInt line[2]
          y = parseInt line[5]
          [x, y] = [y, x] if direction < 0
          @move x, y
    this

  unscramble: (instructions) ->
    @scramble instructions, -1

input = fs.readFileSync 'input.txt', 'utf8'
instructions = input.trim().split '\n'
hashed = 'fbgdceah'
scrambler = new Scrambler hashed
result = scrambler.unscramble instructions
console.log result.toString()