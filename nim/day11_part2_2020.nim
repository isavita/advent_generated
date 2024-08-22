import strutils, sequtils

type
  SeatLayout = seq[seq[char]]

proc readInput(filename: string): SeatLayout =
  let file = readFile(filename)
  let lines = file.splitLines()
  var layout: SeatLayout = newSeq[seq[char]](lines.len)
  for i, line in lines:
    layout[i] = newSeq[char](line.len)
    for j, c in line:
      layout[i][j] = c
  return layout

proc countVisibleOccupiedSeats(layout: SeatLayout, row, col: int): int =
  var count = 0
  let directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
  for dir in directions:
    var r = row + dir[0]
    var c = col + dir[1]
    while r >= 0 and r < layout.len and c >= 0 and c < layout[0].len:
      if layout[r][c] == '#':
        count.inc
        break
      elif layout[r][c] == 'L':
        break
      r += dir[0]
      c += dir[1]
  return count

proc applyRules(layout: SeatLayout): SeatLayout =
  var newLayout: SeatLayout = newSeq[seq[char]](layout.len)
  for i in 0..<layout.len:
    newLayout[i] = newSeq[char](layout[i].len)
    for j in 0..<layout[i].len:
      if layout[i][j] == '.':
        newLayout[i][j] = '.'
      else:
        let occupied = countVisibleOccupiedSeats(layout, i, j)
        if layout[i][j] == 'L' and occupied == 0:
          newLayout[i][j] = '#'
        elif layout[i][j] == '#' and occupied >= 5:
          newLayout[i][j] = 'L'
        else:
          newLayout[i][j] = layout[i][j]
  return newLayout

proc layoutsEqual(layout1, layout2: SeatLayout): bool =
  for i in 0..<layout1.len:
    for j in 0..<layout1[i].len:
      if layout1[i][j] != layout2[i][j]:
        return false
  return true

proc countOccupiedSeats(layout: SeatLayout): int =
  var count = 0
  for row in layout:
    for seat in row:
      if seat == '#':
        count.inc
  return count

let layout = readInput("input.txt")
var currentLayout = layout
var newLayout = applyRules(currentLayout)
while not layoutsEqual(currentLayout, newLayout):
  currentLayout = newLayout
  newLayout = applyRules(currentLayout)

echo countOccupiedSeats(newLayout)