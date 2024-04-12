fs = require 'fs'

fromSnafu = (s) ->
  n = 0
  for i in [0...s.length]
    n *= 5
    switch s[i]
      when '=' then n -= 2
      when '-' then n -= 1
      else n += s.charCodeAt(i) - '0'.charCodeAt(0)
  n

toSnafu = (n) ->
  b = []
  while n > 0
    switch n % 5
      when 3
        n += 5
        b.push '='
      when 4
        n += 5
        b.push '-'
      else
        b.push String.fromCharCode('0'.charCodeAt(0) + n % 5)
    n = Math.floor n / 5
  b.reverse().join ''

data = fs.readFileSync 'input.txt', 'utf8'
lines = data.split '\n'
sum = 0
for line in lines
  sum += fromSnafu line.trim() if line.trim()

result = toSnafu sum
console.log result