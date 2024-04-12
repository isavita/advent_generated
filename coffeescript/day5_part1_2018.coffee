fs = require 'fs'

react = (polymer) ->
  i = 0
  while i < polymer.length - 1
    if polymer[i] isnt polymer[i+1] and (polymer[i].toLowerCase() is polymer[i+1].toLowerCase())
      polymer = polymer[0...i] + polymer[i+2...]
      i = Math.max(0, i - 1)
    else
      i++
  polymer

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
  else
    result = react(data.trim())
    console.log result.length