fs = require 'fs'

class BoardState
  constructor: (board) ->
    @board = board
    @picked = (Array(board.length).fill().map -> Array(board[0].length).fill(false))

  pickNum: (num) ->
    for r in [0...@board.length]
      for c in [0...@board[r].length]
        @picked[r][c] = true if @board[r][c] == num

    for i in [0...@board.length]
      isFullRow = true
      isFullCol = true
      for j in [0...@board.length]
        isFullRow = false unless @picked[i][j]
        isFullCol = false unless @picked[j][i]
      return true if isFullRow or isFullCol
    false

  score: ->
    score = 0
    for r in [0...@board.length]
      for c in [0...@board[r].length]
        score += @board[r][c] unless @picked[r][c]
    score

parseInput = (input) ->
  sections = input.trim().split '\n\n'
  nums = sections[0].split(',').map (n) -> parseInt(n, 10)
  boards = []
  for grid in sections[1..]
    board = []
    for line in grid.split '\n'
      line = line.trim().replace /\s{2,}/g, ' '
      row = line.split(' ').map (n) -> parseInt(n, 10)
      board.push row
    boards.push new BoardState(board)
  [nums, boards]

solve = (input) ->
  [nums, boards] = parseInput(input)
  lastWinningScore = -1
  alreadyWon = {}
  for n in nums
    for bi in [0...boards.length]
      continue if alreadyWon[bi]
      didWin = boards[bi].pickNum(n)
      if didWin
        lastWinningScore = boards[bi].score() * n
        alreadyWon[bi] = true
  lastWinningScore

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  result = solve(data)
  console.log result