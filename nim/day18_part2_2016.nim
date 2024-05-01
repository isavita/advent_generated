import sequtils, strutils

let input = readFile("input.txt").strip()
let rows = 400000
var row = input.toSeq
var safeCount = row.count('.')
var newRow: seq[char]

for _ in 1..rows-1:
  newRow = @[]
  for i in 0..<row.len:
    let left = if i == 0: '.' else: row[i-1]
    let center = row[i]
    let right = if i == row.len-1: '.' else: row[i+1]
    if (left == '^' and center == '^' and right != '^') or
       (center == '^' and right == '^' and left != '^') or
       (left == '^' and center != '^' and right != '^') or
       (right == '^' and center != '^' and left != '^'):
      newRow.add('^')
    else:
      newRow.add('.')
      safeCount.inc
  row = newRow

echo "Safe tiles: ", safeCount