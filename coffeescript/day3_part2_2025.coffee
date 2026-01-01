
fs = require 'fs'
TARGET = 12
total = 0n

for line in fs.readFileSync('input.txt', 'utf8').split /\r?\n/
  s = line.replace /\D+$/,''
  continue if s.length < TARGET
  rem = s.length - TARGET
  stack = []
  for ch in s
    while rem > 0 and stack.length and stack[stack.length - 1] < ch
      stack.pop()
      rem--
    stack.push ch
  total += BigInt stack.slice(0, TARGET).join ''

console.log total.toString()
