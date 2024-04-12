fs = require 'fs'

validatePassword = (policy, password) ->
  [range, char] = policy.split ' '
  [min, max] = range.split('-').map (num) -> parseInt(num, 10)
  char = char[0]
  (password[min - 1] == char) != (password[max - 1] == char)

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  validCount = 0
  lines = data.trim().split '\n'
  for line in lines
    [policy, password] = line.split(': ')
    validCount++ if validatePassword(policy, password)
  console.log validCount