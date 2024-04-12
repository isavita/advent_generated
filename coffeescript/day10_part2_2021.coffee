fs = require 'fs'

getClosingChar = (char) ->
  switch char
    when '(' then ')'
    when '[' then ']'
    when '{' then '}'
    when '<' then '>'
    else ' '

checkAndCompleteLine = (line) ->
  pairings = {')': '(', ']': '[', '}': '{', '>': '<'}
  scoreValues = {')': 1, ']': 2, '}': 3, '>': 4}
  opening = "([{<"
  closing = ")]}>"
  stack = []

  for char in line
    if opening.includes char
      stack.push char
    else if closing.includes char
      return [0, false] if stack.length == 0 or stack[stack.length - 1] isnt pairings[char]
      stack.pop()

  return [0, false] if stack.length == 0

  score = 0
  while stack.length > 0
    openChar = stack.pop()
    score = score * 5 + scoreValues[getClosingChar(openChar)]

  [score, true]

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  scores = []
  lines = data.trim().split '\n'
  for line in lines
    [score, incomplete] = checkAndCompleteLine line
    scores.push score if incomplete

  scores.sort (a, b) -> a - b
  middleScore = scores[Math.floor(scores.length / 2)]
  console.log middleScore