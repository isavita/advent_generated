
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
availablePatterns = input[0].split(',').map (p) -> p.trim()
designs = input.slice(2)

count = 0
for design in designs
  n = design.length
  dp = (false for _ in [0..n])
  dp[0] = true
  for i in [1..n]
    for p in availablePatterns
      lp = p.length
      if i >= lp and dp[i-lp] and design.slice(i-lp, i) == p
        dp[i] = true
        break
  if dp[n]
    count++

console.log count
