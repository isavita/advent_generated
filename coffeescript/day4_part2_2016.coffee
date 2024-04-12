fs = require 'fs'

isRealRoom = (room) ->
  [encryptedName, checksum] = room.split '['
  checksum = checksum.replace ']', ''
  parts = encryptedName.split '-'
  sectorIDPart = parts.pop()
  encryptedName = parts.join ''
  
  letterCounts = {}
  for letter in encryptedName
    letterCounts[letter] = (letterCounts[letter] or 0) + 1

  counts = (for letter, count of letterCounts
    {letter, count}
  ).sort (a, b) ->
    if a.count == b.count
      a.letter.localeCompare b.letter
    else
      b.count - a.count

  valid = true
  for i in [0...checksum.length]
    if checksum[i] isnt counts[i].letter
      valid = false

  valid

getSectorID = (room) ->
  parts = room.split '-'
  sectorIDPart = parts[parts.length - 1]
  parseInt sectorIDPart.split('[')[0]

decryptName = (room) ->
  parts = room.split '-'
  sectorIDPart = parts.pop()
  sectorID = parseInt sectorIDPart.split('[')[0]
  decryptedName = ''

  for part in parts
    for letter in part
      if letter == '-'
        decryptedName += ' '
      else
        shiftedLetter = String.fromCharCode 'a'.charCodeAt(0) + ((letter.charCodeAt(0) - 'a'.charCodeAt(0) + sectorID) % 26)
        decryptedName += shiftedLetter
    decryptedName += ' '

  decryptedName.trim()

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  lines = data.split '\n'
  for line in lines
    if isRealRoom line
      decryptedName = decryptName line
      if decryptedName.includes 'northpole object'
        console.log getSectorID line
        break