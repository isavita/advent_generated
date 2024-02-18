fs = require 'fs'
crypto = require 'crypto'

input = fs.readFileSync('input.txt', 'utf8').trim()

i = 0
hash = ''

while not /^00000/.test(hash)
  i++
  hash = crypto.createHash('md5').update(input + i.toString()).digest('hex')

console.log i
