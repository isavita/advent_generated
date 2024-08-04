fs = require 'fs'

# Read input from file
input = fs.readFileSync 'input.txt', 'utf8'

# Split input into groups
groups = input.trim().split '\n\n'

# Part 1: count questions to which anyone answered "yes"
part1Count = 0
for group in groups
  answers = group.split '\n'
  questions = {}
  for answer in answers
    for char in answer
      questions[char] = true
  part1Count += Object.keys(questions).length
console.log "Part 1:", part1Count

# Part 2: count questions to which everyone answered "yes"
part2Count = 0
for group in groups
  answers = group.split '\n'
  questions = {}
  for answer in answers
    for char in answer
      questions[char] ?= 0
      questions[char]++
  everyoneAnswers = {}
  for char, count of questions
    if count == answers.length
      everyoneAnswers[char] = true
  part2Count += Object.keys(everyoneAnswers).length
console.log "Part 2:", part2Count