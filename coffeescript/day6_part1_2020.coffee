
fs = require 'fs'
readline = require 'readline'

inputFile = './input.txt'
totalCount = 0
groupAnswers = {}

rl = readline.createInterface({
  input: fs.createReadStream(inputFile),
  crlfDelay: Infinity
})

rl.on('line', (line) ->
  if line == ''
    totalCount += Object.keys(groupAnswers).length
    groupAnswers = {}
  else
    for question in line
      groupAnswers[question] = true
)

rl.on('close', ->
  totalCount += Object.keys(groupAnswers).length
  console.log totalCount
)
