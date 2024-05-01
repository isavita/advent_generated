import strutils, sequtils

const totalRows = 40

let file = "input.txt"
let firstRow = readFile(file).strip

proc countChar(str: string, char: char): int =
  for c in str:
    if c == char:
      result.inc

proc isTrap(left, center, right: int, row: string): bool =
  let l = if left < 0 or left >= row.len: '.' else: row[left]
  let c = row[center]
  let r = if right >= row.len: '.' else: row[right]
  (l == '^' and c == '^' and r == '.') or
  (c == '^' and r == '^' and l == '.') or
  (l == '^' and c == '.' and r == '.') or
  (r == '^' and c == '.' and l == '.')

proc countSafeTiles(firstRow: string, totalRows: int): int =
  var currentRow = firstRow
  var safeCount = countChar(currentRow, '.')
  for _ in 1..<totalRows:
    var nextRow = ""
    for j in 0..<currentRow.len:
      if isTrap(j-1, j, j+1, currentRow):
        nextRow &= "^"
      else:
        nextRow &= "."
        safeCount.inc
    currentRow = nextRow
  safeCount

let safeTilesCount = countSafeTiles(firstRow, totalRows)
echo safeTilesCount