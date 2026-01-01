
fs = require 'fs'

isInvalid = (x) ->
  s = x.toString()
  n = s.length
  return false if n <= 1
  for p in [1..Math.floor n/2] when n % p is 0
    k = n / p
    continue if k < 2
    ok = true
    for i in [p...n] when ok
      ok = false if s[i] != s[i % p]
    return true if ok
  false

main = ->
  buf = fs.readFileSync 'input.txt', 'utf8'
  sum = 0n
  i = 0
  len = buf.length
  while i < len
    while i < len and buf[i].match /[ \n\r\t,]/
      i++
    break if i >= len
    a = ''
    while i < len and buf[i].match /[0-9]/
      a += buf[i++]
    break if i >= len or buf[i] != '-'
    i++
    b = ''
    while i < len and buf[i].match /[0-9]/
      b += buf[i++]
    break if a is '' or b is ''
    a = BigInt a
    b = BigInt b
    if a > b then [a, b] = [b, a]
    x = a
    while x <= b
      sum += x if isInvalid x
      break if x is 0xffffffffffffffffn
      x += 1n
  console.log sum.toString()

main()
