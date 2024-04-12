fs = require 'fs'
screenWidth = 50
screenHeight = 6

processInstruction = (instruction, screen) ->
  rectRegex = /^rect (\d+)x(\d+)$/
  rotateRowRegex = /^rotate row y=(\d+) by (\d+)$/
  rotateColumnRegex = /^rotate column x=(\d+) by (\d+)$/

  if rectMatch = instruction.match rectRegex
    a = parseInt rectMatch[1]
    b = parseInt rectMatch[2]
    rect screen, a, b
  else if rotateRowMatch = instruction.match rotateRowRegex
    a = parseInt rotateRowMatch[1]
    b = parseInt rotateRowMatch[2]
    rotateRow screen, a, b
  else if rotateColumnMatch = instruction.match rotateColumnRegex
    a = parseInt rotateColumnMatch[1]
    b = parseInt rotateColumnMatch[2]
    rotateColumn screen, a, b

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

screen = (Array(screenHeight)).fill().map -> Array(screenWidth).fill(false)

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  instructions = data.trim().split '\n'
  for instruction in instructions
    processInstruction instruction, screen

  console.log countLitPixels screen