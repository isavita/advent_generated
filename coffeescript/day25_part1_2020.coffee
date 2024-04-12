fs = require 'fs'

transform = (subjectNumber, loopSize) ->
  value = 1
  for i in [0...loopSize]
    value = (value * subjectNumber) % 20201227
  value

findLoopSize = (publicKey) ->
  value = 1
  loopSize = 0
  while value isnt publicKey
    value = (value * 7) % 20201227
    loopSize++
  loopSize

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  [cardPublicKey, doorPublicKey] = data.trim().split('\n').map (line) -> parseInt(line)
  
  cardLoopSize = findLoopSize(cardPublicKey)
  encryptionKey = transform(doorPublicKey, cardLoopSize)

  console.log encryptionKey