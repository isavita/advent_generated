import strutils, sequtils, sugar, strformat

type BoardState = ref object
  board: seq[seq[int]]
  picked: seq[seq[bool]]

proc newBoardState(board: seq[seq[int]]): BoardState =
  new(result)
  result.board = board
  result.picked = newSeqWith(board.len, newSeq[bool](board[0].len))

proc pickNum(b: BoardState, num: int): bool =
  for r, row in b.board:
    for c, v in row:
      if v == num:
        b.picked[r][c] = true
  for i in 0..<b.board.len:
    var isFullRow = true
    var isFullCol = true
    for j in 0..<b.board[0].len:
      if not b.picked[i][j]:
        isFullRow = false
      if not b.picked[j][i]:
        isFullCol = false
    if isFullRow or isFullCol:
      return true
  return false

proc score(b: BoardState): int =
  for row in b.board:
    for v in row:
      if not b.picked[b.board.find(row)][row.find(v)]:
        result += v

proc parseInput(input: string): (seq[int], seq[BoardState]) =
  let lines = input.split("\n\n")
  var nums: seq[int]
  for v in lines[0].split(","):
    nums.add parseInt(v.strip())
  var boards: seq[BoardState]
  for grid in lines[1..^1]:
    var b: seq[seq[int]]
    for line in grid.split("\n"):
      var row: seq[int]
      for p in line.strip().splitWhitespace():
        row.add parseInt(p)
      b.add row
    boards.add newBoardState(b)
  return (nums, boards)

let input = readFile("input.txt").strip()
let (nums, boards) = parseInput(input)
var lastWinningScore = -1
var alreadyWon: seq[bool]
alreadyWon.setLen(boards.len)
for n in nums:
  for bi, b in boards:
    if alreadyWon[bi]:
      continue
    if b.pickNum(n):
      lastWinningScore = b.score() * n
      alreadyWon[bi] = true
echo lastWinningScore