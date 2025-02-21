
fs = require 'fs'

countWays = (design, patterns) ->
  n = design.length
  dp = (0 for i in [0..n])
  dp[0] = 1
  for i in [1..n]
    for p in patterns
      lp = p.length
      if i >= lp and design.slice(i - lp, i) == p
        dp[i] += dp[i - lp]
  dp[n]

solve = ->
  lines = fs.readFileSync('input.txt').toString().split('\n')
  available_patterns = lines[0].split(',').map (p) -> p.trim()
  total_ways = 0
  for design in lines.slice(2)
    design = design.trim()
    total_ways += countWays(design, available_patterns)
  console.log total_ways

solve()
