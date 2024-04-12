fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  return console.error err if err?
  lines = data.trim().split '\n'
  found = false
  for i in [0...lines.length - 1] when not found
    for j in [i + 1...lines.length] when not found
      diff = 0
      for k in [0...lines[i].length]
        diff++ if lines[i][k] isnt lines[j][k]
        break if diff > 1
      if diff is 1
        common = ''
        for k in [0...lines[i].length]
          common += lines[i][k] if lines[i][k] is lines[j][k]
        console.log common
        found = true