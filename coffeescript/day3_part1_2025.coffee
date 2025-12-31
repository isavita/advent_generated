
fs = require 'fs'

calc = (s) ->
  len = s.length
  return 0 if len is 0
  for d1 in [9..0] by -1
    target = String.fromCharCode 48 + d1
    pos = s.indexOf target
    continue if pos is -1 or pos is len - 1
    max2 = -1
    i = pos + 1
    while i < len
      c = s[i]
      if c >= '0' and c <= '9'
        v = c.charCodeAt(0) - 48
        max2 = v if v > max2
        break if max2 is 9
      i++
    return d1 * 10 + max2 if max2 isnt -1
  0

(() ->
  lines = fs.readFileSync('input.txt', 'utf8').split /\r?\n/
  total = 0
  for line in lines
    total += calc line
  console.log total
)()
