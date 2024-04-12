fs = require 'fs'

isRealRoom = (room) ->
  parts = room.split '['
  checksum = parts[1].slice 0, parts[1].length - 1
  encryptedName = parts[0].split '-'
  encryptedName.pop()
  
  letterCounts = {}
  for part in encryptedName
    for letter in part
      letterCounts[letter] = (letterCounts[letter] or 0) + 1

  counts = (for letter, count of letterCounts
    {letter, count}
  ).sort (a, b) ->
    if a.count == b.count
      a.letter.localeCompare b.letter
    else
      b.count - a.count

  for i in [0...checksum.length]
    return false if checksum[i] isnt counts[i].letter

  true

getSectorID = (room) ->
  parts = room.split '-'
  sectorIDPart = parts[parts.length - 1]
  parseInt sectorIDPart.split('[')[0]

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  sumOfSectorIDs = 0
  data.split('\n').forEach (line) ->
    sumOfSectorIDs += getSectorID(line) if isRealRoom(line)
  console.log sumOfSectorIDs