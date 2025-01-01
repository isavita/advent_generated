
fs = require 'fs'

mod = 1 << 24
numSteps = 2000
patternCount = 19 * 19 * 19 * 19

nextSecret = (s) ->
  x = s * 64
  s ^= x
  s &= mod - 1
  x = Math.floor(s / 32)
  s ^= x
  s &= mod - 1
  x = s * 2048
  s ^= x
  s &= mod - 1
  s

encodeChange4 = (c1, c2, c3, c4) ->
  (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19

try
  input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').filter(Boolean).map(Number)
catch error
  console.error "Error reading input file:", error
  process.exit(1)

buyers = []
for initVal in input
  prices = []
  s = initVal
  for j in [0..numSteps]
    prices.push s % 10
    if j < numSteps
      s = nextSecret(s)
  changes = (prices[i+1] - prices[i] for i in [0...numSteps])
  buyers.push {prices, changes}

globalSum = new Int32Array(patternCount)

for b in buyers
  localPrice = new Int8Array(patternCount)
  localPrice.fill -1
  for i in [0...numSteps - 3]
    c1 = b.changes[i]
    c2 = b.changes[i+1]
    c3 = b.changes[i+2]
    c4 = b.changes[i+3]
    if c1 < -9 or c1 > 9 or c2 < -9 or c2 > 9 or c3 < -9 or c3 > 9 or c4 < -9 or c4 > 9
      continue
    idx = encodeChange4(c1, c2, c3, c4)
    if localPrice[idx] < 0
      localPrice[idx] = b.prices[i+4]
  for idx in [0...patternCount]
    p = localPrice[idx]
    if p >= 0
      globalSum[idx] += p

ans = 0
for s in globalSum
  if s > ans
    ans = s

console.log ans
