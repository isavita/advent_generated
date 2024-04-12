fs = require 'fs'

checkLine = (line) ->
  pairings = {')': '(', ']': '[', '}': '{', '>': '<'}
  scores = {')': 3, ']': 57, '}': 1197, '>': 25137}
  stack = []

  for char in line
    switch char
      when '(', '[', '{', '<'
        stack.push char
      when ')', ']', '}', '>'
        if stack.length == 0 or stack[stack.length - 1] isnt pairings[char]
          return [scores[char], true] # corrupted line
        stack.pop() # pop from stack

  return [0, false] # line is not corrupted

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return

  totalScore = 0
  lines = data.trim().split '\n'
  for line in lines
    [score, corrupted] = checkLine line
    if corrupted
      totalScore += score

  console.log totalScore