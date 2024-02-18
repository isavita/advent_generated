
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim()

lookAndSay = (input, iterations) ->
  current = input
  for i in [0...iterations]
    previousChar = null
    count = 0
    next = ''
    for char in current
      if char isnt previousChar
        if previousChar?
          next += count.toString() + previousChar
        previousChar = char
        count = 1
      else
        count++
    next += count.toString() + previousChar
    current = next
  return current.length

console.log lookAndSay(input, 40)
