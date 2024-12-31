
import strutils, sequtils, os

type Direction = tuple[dx, dy: int]

proc getAllDirections(): seq[Direction] =
  result = @[(0, 1), (1, 0), (1, 1), (-1, 1), (0, -1), (-1, 0), (-1, -1), (1, -1)]

proc checkWord(grid: seq[string], word: string, x, y: int, d: Direction): bool =
  if x < 0 or y < 0 or x >= grid.len or y >= grid[0].len:
    return false
  for i in 0..<word.len:
    let newX = x + (d.dx * i)
    let newY = y + (d.dy * i)
    if newX < 0 or newY < 0 or newX >= grid.len or newY >= grid[0].len:
      return false
    if grid[newX][newY] != word[i]:
      return false
  return true

proc countOccurrences(grid: seq[string], word: string): int =
  var count = 0
  let directions = getAllDirections()
  for i in 0..<grid.len:
    for j in 0..<grid[i].len:
      for dir in directions:
        if checkWord(grid, word, i, j, dir):
          count += 1
  return count

let file = open("input.txt")
var grid: seq[string] = @[]
for line in file.lines:
  let trimmedLine = line.strip()
  if trimmedLine.len > 0:
    grid.add(trimmedLine)
file.close()

let count = countOccurrences(grid, "XMAS")
echo "XMAS appears ", count, " times in the word search"
