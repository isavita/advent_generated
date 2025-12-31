
fs = require 'fs'

lines = fs.readFileSync('input.txt','utf8')
          .split('\n')
          .map (l) -> l.replace(/\r$/,'')

maxw = Math.max 0, ...lines.map (l) -> l.length
isSep = (new Array maxw).fill false

for x in [0...maxw]
  allSpace = true
  for line in lines
    if x < line.length and not /\s/.test line[x]
      allSpace = false
      break
  isSep[x] = allSpace

grandTotal = 0n
processBlock = (start, end) ->
  nums = []
  op = '+'
  for c in [start..end]
    buf = ''
    for line in lines
      if c < line.length
        ch = line[c]
        if /[0-9]/.test ch
          buf += ch
        else if ch is '+' or ch is '*'
          op = ch
    if buf.length then nums.push buf
  return unless nums.length
  blockRes = if op is '*'
    nums.reduce ((a,b) -> a * BigInt(b)), 1n
  else
    nums.reduce ((a,b) -> a + BigInt(b)), 0n
  grandTotal += blockRes

inBlock = false
start = 0
for x in [0...maxw]
  if not isSep[x]
    unless inBlock
      inBlock = true
      start = x
  else if inBlock
    processBlock start, x-1
    inBlock = false
processBlock start, maxw-1 if inBlock

console.log "Grand total: #{grandTotal.toString()}"
