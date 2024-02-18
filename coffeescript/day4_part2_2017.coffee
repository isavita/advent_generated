
fs = require 'fs'

sortString = (w) ->
  s = w.split('')
  s.sort()
  s.join('')

validPassphrase = (passphrase) ->
  wordSet = {}
  words = passphrase.split(' ')
  for word in words
    sortedWord = sortString(word)
    if wordSet[sortedWord]
      return false
    wordSet[sortedWord] = true
  return true

countValidPassphrases = ->
  data = fs.readFileSync('input.txt', 'utf8')
  passphrases = data.trim().split('\n')
  validCount = 0
  for passphrase in passphrases
    if validPassphrase(passphrase)
      validCount++
  console.log validCount

countValidPassphrases()
