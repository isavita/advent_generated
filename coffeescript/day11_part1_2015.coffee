fs = require 'fs'

findNextPassword = (password) ->
  password = incrementPassword password while not isValidPassword password
  password

incrementPassword = (password) ->
  chars = password.split ''
  i = chars.length - 1
  while i >= 0
    chars[i] = String.fromCharCode(chars[i].charCodeAt(0) + 1)
    if chars[i] > 'z'
      chars[i] = 'a'
      i -= 1
    else
      break
  chars.join ''

isValidPassword = (password) ->
  hasStraight(password) and not containsInvalidLetters(password) and hasTwoPairs(password)

hasStraight = (password) ->
  for i in [0...password.length - 2]
    if password.charCodeAt(i) + 1 == password.charCodeAt(i + 1) and password.charCodeAt(i) + 2 == password.charCodeAt(i + 2)
      return true
  false

containsInvalidLetters = (password) ->
  for char in password
    return true if char in ['i', 'o', 'l']
  false

hasTwoPairs = (password) ->
  count = 0
  i = 0
  while i < password.length - 1
    if password[i] == password[i + 1]
      count += 1
      i += 1
    i += 1
  count >= 2

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading input:", err
    return
  newPassword = findNextPassword data.trim()
  console.log newPassword