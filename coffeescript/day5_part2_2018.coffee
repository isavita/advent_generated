# Read input from file
fs = require 'fs'
input = fs.readFileSync 'input.txt', 'utf8'

# Part 1: Fully react the polymer
react = (polymer) ->
  stack = []
  for char in polymer
    if stack.length > 0 and stack[stack.length - 1].toLowerCase() is char.toLowerCase() and stack[stack.length - 1] isnt char
      stack.pop()
    else
      stack.push char
  stack.length

console.log "Part 1: #{react input}"

# Part 2: Remove all instances of a unit type and fully react the remaining polymer
removeUnit = (polymer, unit) ->
  polymer.replace new RegExp(unit, 'gi'), ''

minLength = Infinity
for unit in 'abcdefghijklmnopqrstuvwxyz'
  length = react removeUnit input, unit
  if length < minLength
    minLength = length

console.log "Part 2: #{minLength}"