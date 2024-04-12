fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  expenses = (parseInt(line) for line in data.trim().split '\n')
  expenses = expenses.filter (num) -> num?

  for i in [0...expenses.length]
    for j in [i+1...expenses.length]
      for k in [j+1...expenses.length]
        if expenses[i] + expenses[j] + expenses[k] == 2020
          console.log expenses[i] * expenses[j] * expenses[k]
          return