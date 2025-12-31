
fs = require 'fs'

main = ->
  content = fs.readFileSync 'input.txt', 'utf8'
  pow10 = [1n]
  for i in [1..20] then pow10[i] = pow10[i-1] * 10n

  ids = []

  process = (tok) ->
    return unless tok.length
    dash = tok.indexOf '-'
    return if dash is -1
    a = tok.slice 0, dash
    b = tok.slice dash + 1
    return if a.length is 0 or b.length is 0
    start = BigInt a
    end = BigInt b
    if start > end then [start, end] = [end, start]
    for k in [1..10]
      mult = pow10[k] + 1n
      minSeed = pow10[k-1]
      maxSeed = pow10[k] - 1n
      sMin = (start + mult - 1n) / mult
      sMax = end / mult
      sMin = minSeed if sMin < minSeed
      sMax = maxSeed if sMax > maxSeed
      continue if sMin > sMax
      seed = sMin
      while seed <= sMax
        ids.push seed * mult
        seed += 1n

  token = ''
  for c in content
    if c >= '0' and c <= '9' or c is '-'
      token += c
    else if token.length
      process token
      token = ''
  process token if token.length

  ids.sort (a,b) -> if a<b then -1 else if a>b then 1 else 0
  sum = 0n
  prev = null
  for v in ids
    unless v is prev
      sum += v
      prev = v
  console.log sum.toString()

main()
