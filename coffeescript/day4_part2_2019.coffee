
fs = require 'fs'

isValidPassword = (password) ->
  s = password.toString()
  hasDouble = false

  for i in [0...s.length - 1]
    if s[i] > s[i + 1]
      return false

    if s[i] == s[i + 1]
      if (i == 0 || s[i] != s[i - 1]) and (i + 2 >= s.length or s[i] != s[i + 2])
        hasDouble = true

  return hasDouble

rangeStr = fs.readFileSync('input.txt', 'utf8').trim()
ranges = rangeStr.split('-')
start = parseInt(ranges[0])
end = parseInt(ranges[1])

count = 0
for i in [start...end + 1]
  if isValidPassword(i)
    count++

console.log(count)
