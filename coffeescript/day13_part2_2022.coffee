fs = require 'fs'

readAll = (path) ->
  fs.readFileSync(path, 'utf8')

compare = (a, b) ->
  anum = typeof a == 'number'
  bnum = typeof b == 'number'
  switch
    when anum && bnum
      sign(a - b)
    when anum
      compare([a], b)
    when bnum
      compare(a, [b])
    else
      aa = a
      bb = b
      for i in [0...Math.min(aa.length, bb.length)]
        c = compare(aa[i], bb[i])
        return c if c != 0
      sign(aa.length - bb.length)

sign = (n) ->
  if n == 0 then 0 else if n < 0 then -1 else 1

s = readAll 'input.txt'
packets = []
for pair in s.split '\n\n'
  sp = pair.split '\n'
  first = JSON.parse sp[0]
  second = JSON.parse sp[1]
  packets.push first, second

divider1 = JSON.parse '[[2]]'
divider2 = JSON.parse '[[6]]'
packets.push divider1, divider2
packets.sort (a, b) -> compare a, b

divider1Pos = packets.findIndex (pkt) -> compare(pkt, divider1) >= 0
divider2Pos = packets.findIndex (pkt) -> compare(pkt, divider2) >= 0

console.log (divider1Pos + 1) * (divider2Pos + 1)