
fs = require('fs')

getUniqueIndex = (input, n) ->
  for i in [n...input.length]
    chars = input[i - n...i]
    return i if new Set(chars).size is n
  -1

readAll = (path) ->
  input = fs.readFileSync(path, 'utf8')
  input.trim()

firstNUnique = (input, n) -> getUniqueIndex(input, n)

input = readAll('input.txt')
console.log(firstNUnique(input, 14))
