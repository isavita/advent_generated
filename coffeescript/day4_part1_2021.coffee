fs = require 'fs'

class BingoBoard
  constructor: (@numbers) ->
    @marked = (Array(5).fill().map -> Array(5).fill(false))

  mark: (number) ->
    for i in [0...@numbers.length]
      for j in [0...@numbers[i].length]
        @marked[i][j] = true if @numbers[i][j] == number

  hasWon: ->
    for i in [0...@marked.length]
      return true if isRowMarked(@marked[i]) or isColumnMarked(@marked, i)
    false

  unmarkedSum: ->
    sum = 0
    for i in [0...@numbers.length]
      for j in [0...@numbers[i].length]
        sum += @numbers[i][j] unless @marked[i][j]
    sum

isRowMarked = (row) ->
  not row.includes(false)

isColumnMarked = (marked, column) ->
  for row in marked
    return false unless row[column]
  true

input = fs.readFileSync('input.txt', 'utf8').split('\n')
numbers = input[0].split(',').map(Number)
boards = []

i = 2
while i < input.length
  if input[i].trim().length == 0
    i += 1
    continue
  numbersMatrix = []
  for j in [0...5]
    numbersMatrix.push(input[i + j].trim().split(/\s+/).map(Number))
  boards.push(new BingoBoard(numbersMatrix))
  i += 6

winningBoard = null
winningNumber = null
for number in numbers
  break if winningBoard?
  for board in boards
    board.mark(number)
    if board.hasWon()
      winningBoard = board
      winningNumber = number
      break

console.log winningBoard.unmarkedSum() * winningNumber if winningBoard?