fs = require 'fs'

parseInput = (input) ->
  histories = []
  for line in input
    numbers = parseStringToInts(line)
    histories.push numbers
  histories

parseStringToInts = (numbersLine) ->
  numbers = []
  numbersParts = numbersLine.trim().split /\s+/
  for numberStr in numbersParts
    number = parseInt numberStr, 10
    if isNaN(number)
      throw new Error "Invalid number in input"
    numbers.push number
  numbers

allZeros = (nums) ->
  for num in nums
    return false if num isnt 0
  true

calculateExtrapolation = (history) ->
  extrapolations = []
  for i in [1...history.length]
    extrapolation = history[i] - history[i - 1]
    extrapolations.push extrapolation
  extrapolations

calculateExtrapolations = (history) ->
  extrapolationsSeries = []
  extrapolationsSeries.push history

  for i in [1...history.length]
    previousExtrapolations = extrapolationsSeries[i - 1]
    return extrapolationsSeries if allZeros(previousExtrapolations)

    extrapolations = calculateExtrapolation(previousExtrapolations)
    extrapolationsSeries.push extrapolations

  extrapolationsSeries

solve = (input) ->
  histories = parseInput(input)
  res = 0

  for history in histories
    extrapolationsSeries = calculateExtrapolations(history)

    pastPrediction = 0
    for i in [(extrapolationsSeries.length - 1)..0]
      pastPrediction = extrapolationsSeries[i][0] - pastPrediction

    res += pastPrediction

  res

readFile = (fileName) ->
  file = fs.readFileSync fileName, 'utf8'
  file.trim().split '\n'

main = ->
  input = readFile "input.txt"
  console.log solve(input)

main()