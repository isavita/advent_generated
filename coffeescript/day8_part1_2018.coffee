fs = require 'fs'

readInput = (filename) ->
  data = fs.readFileSync(filename, 'utf8')
  data.split(' ').map (num) -> parseInt(num, 10)

parseTree = (data, index) ->
  childCount = data[index]
  metaCount = data[index+1]
  index += 2
  
  sum = 0
  for i in [0...childCount]
    [childSum, index] = parseTree(data, index)
    sum += childSum
  
  for i in [0...metaCount]
    sum += data[index + i]
  index += metaCount
  
  [sum, index]

do ->
  numbers = readInput('input.txt')
  [result, _] = parseTree(numbers, 0)
  console.log result