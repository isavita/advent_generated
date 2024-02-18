
fs = require('fs')
crypto = require('crypto')

findPassword = (doorID) ->
  password = ''
  i = 0
  while password.length < 8
    hash = md5Hash(doorID + i)
    if hash.startsWith '00000'
      password += hash[5]
    i++
  return password

md5Hash = (input) ->
  h = crypto.createHash('md5')
  h.update input
  return h.digest('hex')

doorID = fs.readFileSync('input.txt', 'utf8').trim()
password = findPassword(doorID)
console.log password
