fs = require 'fs'
crypto = require 'crypto'

input = fs.readFileSync('input.txt', 'utf8').trim()

fiveZeroes = ->
  i = 0
  while true
    hash = crypto.createHash('md5').update("#{input}#{++i}").digest('hex')
    return i if hash.startsWith '00000'

sixZeroes = ->
  i = 0
  while true
    hash = crypto.createHash('md5').update("#{input}#{++i}").digest('hex')
    return i if hash.startsWith '000000'

console.log fiveZeroes()
console.log sixZeroes()