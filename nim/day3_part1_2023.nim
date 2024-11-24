
import std/[strutils, sequtils, unicode]

proc readFileToMatrix(filePath: string): seq[seq[char]] =
  let content = readFile(filePath)
  return content.splitLines().mapIt(it.toSeq())

proc extractNumber(matrix: seq[seq[char]], x, y: int): (int, int) =
  var 
    numberStr = ""
    currentX = x
  while currentX < matrix[y].len and matrix[y][currentX].isDigit:
    numberStr.add matrix[y][currentX]
    inc currentX
  return (parseInt(numberStr), numberStr.len)

proc checkAdjacent(matrix: seq[seq[char]], x, y: int): bool =
  for dy in -1..1:
    for dx in -1..1:
      let 
        adjX = x + dx
        adjY = y + dy
      if adjY >= 0 and adjY < matrix.len and 
         adjX >= 0 and adjX < matrix[adjY].len:
        let cell = matrix[adjY][adjX]
        if not cell.isDigit and cell != '.':
          return true
  return false

proc isAdjacentToSymbol(matrix: seq[seq[char]], x, y, length: int): bool =
  for i in 0..<length:
    if checkAdjacent(matrix, x+i, y):
      return true
  return false

proc sumOfPartNumbers(matrix: seq[seq[char]]): int =
  var visited = newSeqWith(matrix.len, newSeq[bool](matrix[0].len))
  
  for y in 0..<matrix.len:
    var x = 0
    while x < matrix[y].len:
      if not visited[y][x] and matrix[y][x].isDigit:
        let (number, length) = extractNumber(matrix, x, y)
        if isAdjacentToSymbol(matrix, x, y, length):
          result += number
        
        # Mark digits as visited
        for i in 0..<length:
          visited[y][x+i] = true
        
        x += length
      else:
        inc x

proc main() =
  let matrix = readFileToMatrix("input.txt")
  echo sumOfPartNumbers(matrix)

main()
