fs = require 'fs'

hasDoubleAndIncreasingDigits = (s) ->
  hasDouble = false
  for i in [0...s.length - 1]
    hasDouble = true if s[i] == s[i + 1]
    return false if s[i] > s[i + 1]
  hasDouble

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  [start, end] = data.trim().split('-').map (num) -> parseInt num, 10

  count = 0
  for i in [start..end]
    count++ if hasDoubleAndIncreasingDigits i.toString()

  console.log count