fs = require 'fs'

data = fs.readFileSync 'input.txt', 'utf8'
lines = data.trim().split '\n'

genAStart = parseInt lines[0]
genBStart = parseInt lines[1]

genAFactor = 16807
genBFactor = 48271
modulus = 2147483647

genA = genAStart
genB = genBStart
matches = 0

for i in [0...40000000]
  genA = (genA * genAFactor) % modulus
  genB = (genB * genBFactor) % modulus

  if (genA & 0xFFFF) == (genB & 0xFFFF)
    matches++

console.log matches