import strutils, sequtils

let input = "input.txt".readFile().strip().splitLines()

var x, trees = 0

for row in input:
  if row[x mod len(row)] == '#':
    trees += 1
  x += 3

echo "Trees encountered: ", trees