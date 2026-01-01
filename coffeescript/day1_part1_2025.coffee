
fs = require 'fs'

main = ->
  tokens = fs.readFileSync('input.txt', 'utf8').trim().split /\s+/
  pos = 50
  cnt = 0
  for t in tokens when t.length
    dir = t[0]
    amt = +t.slice 1
    pos = (pos + (if dir is 'R' then amt else -amt)) % 100
    pos += 100 if pos < 0
    cnt++ if pos is 0
  console.log cnt

main()
