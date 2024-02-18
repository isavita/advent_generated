
fs = require 'fs'

firstNUnique = (s, n) ->
  for i in [n...s.length]
    b = s[i-n...i]
    if new Set(b).size is n
      return i
  return -1

readAll = (path) ->
  try
    file = fs.readFileSync(path, 'utf8').trim()
  catch err
    throw err
  return file

solution = ->
  s = readAll('input.txt')
  console.log(firstNUnique(s, 4))

solution()
