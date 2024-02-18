
fs = require('fs')

try
  data = fs.readFileSync('input.txt', 'utf8')
catch error
  console.log 'File reading error:', error
  process.exit(1)

passphrases = data.trim().split('\n')
validCount = 0

for passphrase in passphrases
  words = passphrase.split(' ')
  wordSet = {}
  valid = true
  for word in words
    if wordSet[word]
      valid = false
      break
    wordSet[word] = true

  if valid
    validCount++

console.log validCount
