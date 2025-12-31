
fs = require 'fs'

# ---------- big‑int helpers ----------
add = (a, b) -> a + b
mul = (a, b) -> a * b

# ---------- block processing ----------
processBlock = (sc, ec, lines, grand) ->
  nums = []
  op   = 0                       # 0 – none, 1 – '+', 2 – '*'

  for line in lines
    seg = line[sc..ec] ? ''      # slice column range, safe for short lines
    seg = seg.replace /^\s+|\s+$/g, ''   # trim

    continue if seg.length is 0

    if seg is '+'
      op = 1
    else if seg is '*'
      op = 2
    else
      nums.push seg

  return if nums.length is 0

  acc = if op is 1
          nums.reduce ((a, s) -> add a, BigInt(s)), 0n
        else if op is 2
          nums.reduce ((a, s) -> mul a, BigInt(s)), 1n
        else
          BigInt nums[0]

  grand + acc

# ---------- main ----------
main = ->
  lines = fs.readFileSync('input.txt', 'utf8')
            .split /\r?\n/
  maxw  = lines.reduce ((m, l) -> Math.max m, l.length), 0

  grand = 0n
  inBlock = false
  startCol = 0

  for x in [0...maxw]
    sep = true
    for L in lines
      if x < L.length and not /\s/.test L[x]
        sep = false
        break

    if not sep
      unless inBlock
        inBlock = true
        startCol = x
    else if inBlock
      grand = processBlock startCol, x-1, lines, grand
      inBlock = false

  # trailing block
  if inBlock
    grand = processBlock startCol, maxw-1, lines, grand

  console.log "Grand total: #{grand.toString()}"

main()
