
fs = require 'fs'

parseLock = (b) ->
  (b[r][c] == '#' for r in [1...7]).filter((x) -> x).length for c in [0...5]

parseKey = (b) ->
  (b[r][c] == '#' for r in [5...-1]).filter((x) -> x).length for c in [0...5]

fits = (lock, key) ->
  (lock[i] + key[i] <= 5 for i in [0...5]).every (x) -> x

solve = ->
  raw = fs.readFileSync('input.txt', 'utf8').trim().split('\n').filter (line) -> line.trim().length > 0
  return console.log 0 if raw.length % 7 != 0
  locks = []
  keys = []
  for i in [0...raw.length] by 7
    block = raw[i...i + 7]
    continue if (ln.length < 5 for ln in block).some (x) -> x
    if (c == '#' for c in block[0]).every (x) -> x
      locks.push parseLock block
    else
      keys.push parseKey block
  count = 0
  for lock in locks
    count += 1 for key in keys when fits lock, key
  console.log count

solve()
