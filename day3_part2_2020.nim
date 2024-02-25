
import os
import strutils

var
  file = open("input.txt")
  lines = file.readAll.splitLines

let slopes = @[
  @[1, 1],
  @[3, 1],
  @[5, 1],
  @[7, 1],
  @[1, 2]
]

var product = 1
for slope in slopes:
  var treeCount = 0
  var pos = 0
  for i in countup(0, lines.len-1, slope[1]):
    if lines[i][pos] == '#':
      inc(treeCount)
    pos = (pos + slope[0]) mod lines[i].len
  product *= treeCount

echo product
