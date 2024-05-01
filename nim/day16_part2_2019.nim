import strutils, sequtils

let file = readFile("input.txt")
let input = file.strip()
var repeatedInput = newSeq[int](input.len * 10000)
for t in 0..<10000:
  for i, r in input:
    repeatedInput[t*input.len + i] = parseInt($r)

let offset = parseInt(input[0..6])
for phase in 0..<100:
  var sum = 0
  for i in countdown(repeatedInput.len-1, offset):
    sum += repeatedInput[i]
    repeatedInput[i] = sum mod 10

for i in offset..<offset+8:
  write(stdout, $repeatedInput[i])
echo()