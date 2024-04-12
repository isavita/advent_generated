crypto = require 'crypto'
fs = require 'fs'

findPassword = (doorID) ->
  password = Array(8)
  filledPositions = 0
  found = Array(8).fill(false)

  i = 0
  while filledPositions < 8
    hash = md5Hash(doorID + i)
    if hash.startsWith('00000')
      pos = hash[5]
      if pos >= '0' and pos <= '7'
        posIndex = parseInt(pos)
        if not found[posIndex]
          found[posIndex] = true
          password[posIndex] = hash[6]
          filledPositions++

    i++
  password.join('')

md5Hash = (input) ->
  hash = crypto.createHash('md5')
  hash.update(input)
  hash.digest('hex')

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  doorID = data.trim()
  password = findPassword(doorID)
  console.log password