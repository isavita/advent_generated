
fs = require 'fs'

main = ->
  # Read and split into nonempty blocks of 3 lines each
  raw  = fs.readFileSync 'input.txt', 'utf8'
  lines = raw.split /\r?\n/
  machines = []
  block = []
  for line in lines
    if line.trim() == ''
      if block.length
        machines.push block
        block = []
    else
      block.push line
  # last block
  machines.push block if block.length

  parseMachine = (blk) ->
    # blk[0] = "Button A: X+94, Y+34"
    # blk[1] = "Button B: X+22, Y+67"
    # blk[2] = "Prize: X=8400, Y=5400"
    [, dx1, dy1] = blk[0].match /X\+(-?\d+), Y\+(-?\d+)/
    [, dx2, dy2] = blk[1].match /X\+(-?\d+), Y\+(-?\d+)/
    [, X,  Y ] = blk[2].match /X=(-?\d+), Y=(-?\d+)/
    {
      dx1: +dx1, dy1: +dy1
      dx2: +dx2, dy2: +dy2
      X:  +X,  Y:  +Y
    }

  solveFor = (dx1, dy1, dx2, dy2, X, Y, maxPresses = null) ->
    # Solve:
    #   a*dx1 + b*dx2 = X
    #   a*dy1 + b*dy2 = Y
    det = dx1*dy2 - dx2*dy1
    return null if det == 0
    a = (X*dy2 - Y*dx2) / det
    b = (dx1*Y - dy1*X) / det
    # require integer and nonnegative
    return null unless Number.isInteger(a) and Number.isInteger(b)
    return null if a < 0 or b < 0
    if maxPresses?
      return null if a > maxPresses or b > maxPresses
    # cost = 3 tokens per A press, 1 per B press
    cost = 3*a + b
    { a, b, cost }

  part1Cost = 0
  part1Count = 0

  part2Cost = 0
  part2Count = 0
  OFFSET = 10000000000000

  for blk in machines
    {dx1, dy1, dx2, dy2, X, Y} = parseMachine blk

    # Part 1: limit 100 presses each
    sol1 = solveFor dx1, dy1, dx2, dy2, X, Y, 100
    if sol1?
      part1Count += 1
      part1Cost  += sol1.cost

    # Part 2: add offset, no press limit
    sol2 = solveFor dx1, dy1, dx2, dy2, X+OFFSET, Y+OFFSET
    if sol2?
      part2Count += 1
      part2Cost  += sol2.cost

  console.log "Part 1: You can win #{part1Count} prizes for #{part1Cost} tokens (≤100 presses)."
  console.log "Part 2: You can win #{part2Count} prizes for #{part2Cost} tokens (with +10^13 offset)."

# run
main()
