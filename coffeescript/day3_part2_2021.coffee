fs = require 'fs'

filterValues = (values, criteria) ->
  for i in [0...values[0].length]
    zeros = ones = 0
    for val in values
      if val[i] == '0'
        zeros++
      else
        ones++
    keep = criteria zeros, ones
    values = filterByBit values, i, keep
    break if values.length == 1
  values[0]

filterByBit = (values, bitIndex, keep) ->
  filtered = []
  for val in values
    filtered.push val if val[bitIndex] == keep
  filtered

data = fs.readFileSync('input.txt', 'utf8').trim().split '\n'

oxygenGeneratorRating = filterValues data, (zeros, ones) ->
  if zeros > ones then '0' else '1'

co2ScrubberRating = filterValues data, (zeros, ones) ->
  if zeros <= ones then '0' else '1'

oxygenGeneratorRatingInt = parseInt oxygenGeneratorRating, 2
co2ScrubberRatingInt = parseInt co2ScrubberRating, 2

console.log oxygenGeneratorRatingInt * co2ScrubberRatingInt