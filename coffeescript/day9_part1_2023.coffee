fs = require 'fs'

parseStringToInts = (numbersLine) ->
  numbers = []
  numbersParts = numbersLine.trim().split /\s+/
  for numberStr in numbersParts
    number = parseInt numberStr
    throw new Error('Parsing error') unless number?
    numbers.push number
  numbers

allZeros = (nums) ->
  for num in nums
    return false if num isnt 0
  true

calculateExtrapolation = (history) ->
  extrapolations = []
  for i in [1...history.length]
    extrapolation = history[i] - history[i-1]
    extrapolations.push extrapolation
  extrapolations

calculateExtrapolations = (history) ->
  extrapolationsSeries = []
  extrapolationsSeries.push history

  for i in [1...history.length]
    previousExtrapolations = extrapolationsSeries[i-1]
    return extrapolationsSeries if allZeros(previousExtrapolations)

    extrapolations = calculateExtrapolation previousExtrapolations
    extrapolationsSeries.push extrapolations

  extrapolationsSeries

solve = (input) ->
  histories = (parseStringToInts(line) for line in input)
  res = 0

  for history in histories
    extrapolationsSeries = calculateExtrapolations history

    futurePrediction = 0
    for i in [(extrapolationsSeries.length-1)..0]
      futurePrediction += extrapolationsSeries[i][extrapolationsSeries[i].length-1]

    res += futurePrediction

  res

readFile = (fileName) ->
  file = fs.readFileSync fileName, 'utf8'
  file.trim().split '\n'

main = ->
  input = readFile "input.txt"
  console.log solve(input)

main()