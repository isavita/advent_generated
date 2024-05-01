import sequtils, strutils

let programs = toSeq('a'..'p')
var order = programs.toSeq

for line in "input.txt".lines:
  let parts = line.split(',')
  for move in parts:
    if move.startsWith("s"):
      let spin = move[1..^1].parseInt
      order = order[order.len - spin ..< order.len] & order[0 ..< order.len - spin]
    elif move.startsWith("x"):
      let indices = move[1..^1].split('/')
      let i = indices[0].parseInt
      let j = indices[1].parseInt
      swap(order[i], order[j])
    elif move.startsWith("p"):
      let programs = move[1..^1].split('/')
      let i = order.find(programs[0][0])
      let j = order.find(programs[1][0])
      swap(order[i], order[j])

echo order.join("")