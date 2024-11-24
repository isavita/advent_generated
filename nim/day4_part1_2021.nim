
import std/[strutils, sequtils, parseutils]

type
  BingoBoard = ref object
    numbers: seq[seq[int]]
    marked: seq[seq[bool]]

proc mark(board: var BingoBoard, number: int) =
  for i in 0..<board.numbers.len:
    for j in 0..<board.numbers[i].len:
      if board.numbers[i][j] == number:
        board.marked[i][j] = true

proc hasWon(board: BingoBoard): bool =
  # Check rows
  for row in board.marked:
    if row.all(proc(x: bool): bool = x):
      return true
  
  # Check columns
  for j in 0..<board.marked[0].len:
    if board.marked.all(proc(row: seq[bool]): bool = row[j]):
      return true
  
  false

proc unmarkedSum(board: BingoBoard): int =
  for i in 0..<board.numbers.len:
    for j in 0..<board.numbers[i].len:
      if not board.marked[i][j]:
        result += board.numbers[i][j]

proc main() =
  let input = readFile("input.txt").strip().split("\n")
  
  # Parse drawn numbers
  let numbers = input[0].split(",").map(parseInt)
  
  # Parse boards
  var boards: seq[BingoBoard] = @[]
  var i = 2
  while i < input.len:
    var board = BingoBoard(
      numbers: newSeq[seq[int]](5),
      marked: newSeq[seq[bool]](5)
    )
    
    for row in 0..<5:
      board.numbers[row] = input[i+row].splitWhitespace().map(parseInt)
      board.marked[row] = newSeq[bool](5)
    
    boards.add(board)
    i += 6

  # Play bingo
  var winningBoard: BingoBoard
  var winningNumber: int
  
  for number in numbers:
    for board in boards.mitems:
      board.mark(number)
      if board.hasWon():
        winningBoard = board
        winningNumber = number
        break
    
    if winningBoard != nil:
      break

  echo winningBoard.unmarkedSum() * winningNumber

main()
