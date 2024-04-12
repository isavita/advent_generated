fs = require 'fs'
crypto = require 'crypto'

hashCache = {}

findTriplet = (hash) ->
  for i in [0...hash.length - 2]
    return hash[i] if hash[i] == hash[i+1] and hash[i] == hash[i+2]
  ""

getMD5Hash = (input) ->
  crypto.createHash('md5').update(input).digest('hex')

getStretchedMD5Hash = (input) ->
  return hashCache[input] if hashCache[input]
  hash = getMD5Hash(input)
  for i in [0...2016]
    hash = getMD5Hash(hash)
  hashCache[input] = hash
  hash

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  salt = data.trim()
  keys = 0
  index = 0
  while keys < 64
    hash = getStretchedMD5Hash(salt + index.toString())
    triplet = findTriplet(hash)
    if triplet
      for i in [1...1001]
        nextHash = getStretchedMD5Hash(salt + (index + i).toString())
        if nextHash.indexOf(triplet.repeat(5)) isnt -1
          keys++
          break
    index++
  console.log index - 1