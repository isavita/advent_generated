fs = require 'fs'

parseInput = (data) ->
  [genAStart, genBStart] = data.trim().split("\n").map (x) -> parseInt(x, 10)

  genAFactor = 16807
  genBFactor = 48271
  modulus = 2147483647

  genA = genAStart
  genB = genBStart
  matches = 0

  for i in [0...5000000]
    # Generate next value for A that is a multiple of 4
    while true
      genA = (genA * genAFactor) % modulus
      break if genA % 4 == 0

    # Generate next value for B that is a multiple of 8
    while true
      genB = (genB * genBFactor) % modulus
      break if genB % 8 == 0

    # Check if the lowest 16 bits are equal
    matches++ if (genA & 0xFFFF) == (genB & 0xFFFF)

  console.log matches

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return
  parseInput(data)