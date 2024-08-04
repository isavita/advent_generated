fs = require 'fs'

currentPassword = fs.readFileSync('input.txt', 'utf8').trim()

findNextPassword = (password) ->
  while true
    password = incrementPassword(password)
    return password if isValidPassword(password)

incrementPassword = (password) ->
  runes = password.split('')
  for i in [runes.length - 1..0]
    runes[i] = String.fromCharCode(runes[i].charCodeAt(0) + 1)
    if runes[i] > 'z'
      runes[i] = 'a'
      continue
    break
  runes.join('')

isValidPassword = (password) ->
  hasStraight(password) and not containsInvalidLetters(password) and hasTwoPairs(password)

hasStraight = (password) ->
  for i in [0...password.length - 2]
    return true if password.charCodeAt(i) + 1 == password.charCodeAt(i + 1) and password.charCodeAt(i) + 2 == password.charCodeAt(i + 2)
  false

containsInvalidLetters = (password) ->
  for c in password
    return true if c in ['i', 'o', 'l']
  false

hasTwoPairs = (password) ->
  count = 0
  i = 0
  while i < password.length - 1
    if password[i] == password[i + 1]
      count++
      i += 2 # Skip the next character
    else
      i++
  count >= 2

secondNewPassword = findNextPassword(findNextPassword(currentPassword))
console.log secondNewPassword