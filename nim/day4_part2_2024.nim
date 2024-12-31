
import strutils, sequtils, os

proc checkMAS(grid: seq[string], x, y, dx, dy: int): bool =
  if x < 0 or y < 0 or x >= grid.len or y >= grid[0].len:
    return false
  let word = "MAS"
  var forward = true
  var backward = true
  for i in 0..<word.len:
    let newX = x + dx * i
    let newY = y + dy * i
    if newX < 0 or newY < 0 or newX >= grid.len or newY >= grid[0].len:
      forward = false
      break
    if grid[newX][newY] != word[i]:
      forward = false
  for i in 0..<word.len:
    let newX = x + dx * i
    let newY = y + dy * i
    if newX < 0 or newY < 0 or newX >= grid.len or newY >= grid[0].len:
      backward = false
      break
    if grid[newX][newY] != word[word.len - 1 - i]:
      backward = false
  return forward or backward

proc checkXMAS(grid: seq[string], x, y: int): bool =
  if checkMAS(grid, x - 1, y - 1, 1, 1) and checkMAS(grid, x - 1, y + 1, 1, -1):
    return true
  if checkMAS(grid, x + 1, y - 1, -1, 1) and checkMAS(grid, x + 1, y + 1, -1, -1):
    return true
  return false

proc countXMASPatterns(grid: seq[string]): int =
  if grid.len < 3 or grid[0].len < 3:
    return 0
  var count = 0
  for i in 1..<grid.len - 1:
    for j in 1..<grid[i].len - 1:
      if grid[i][j] == 'A' and checkXMAS(grid, i, j):
        inc count
  return count

let lines = readFile("input.txt").splitLines().filterIt(it.len > 0)
let count = countXMASPatterns(lines)
echo "X-MAS patterns appear ", count, " times in the word search"
