fs = require 'fs'

screenWidth = 50
screenHeight = 6

displayScreen = (screen) ->
  for row in screen
    console.log row.map((pixel) -> if pixel then '#' else '.').join ''

processInstruction = (instruction, screen) ->
  rectRegex = /rect (\d+)x(\d+)/
  rotateRowRegex = /rotate row y=(\d+) by (\d+)/
  rotateColumnRegex = /rotate column x=(\d+) by (\d+)/

  if matches = instruction.match(rectRegex)
    [_, a, b] = matches
    rect screen, parseInt(a), parseInt(b)
  else if matches = instruction.match(rotateRowRegex)
    [_, a, b] = matches
    rotateRow screen, parseInt(a), parseInt(b)
  else if matches = instruction.match(rotateColumnRegex)
    [_, a, b] = matches
    rotateColumn screen, parseInt(a), parseInt(b)

rect = (screen, a, b) ->
  for y in [0...b]
    for x in [0...a]
      screen[y][x] = true

rotateRow = (screen, row, shift) ->
  temp = new Array(screenWidth)
  for i in [0...screenWidth]
    temp[(i + shift) % screenWidth] = screen[row][i]
  screen[row] = temp

rotateColumn = (screen, col, shift) ->
  temp = new Array(screenHeight)
  for i in [0...screenHeight]
    temp[(i + shift) % screenHeight] = screen[i][col]
  for i in [0...screenHeight]
    screen[i][col] = temp[i]

countLitPixels = (screen) ->
  count = 0
  for row in screen
    for pixel in row
      count++ if pixel
  count

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  screen = (new Array(screenHeight).fill(0).map -> new Array(screenWidth).fill(false))
  for instruction in data.split '\n'
    processInstruction instruction, screen
  displayScreen screen
  console.log countLitPixels screen