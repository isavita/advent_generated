
fs = require('fs')

readInputFile = (callback) ->
  fs.readFile 'input.txt', 'utf8', (err, data) ->
    if err
      console.error err
      process.exit(1)
    else
      messages = data.split('\n')
      callback messages

getOriginalMessage = (messages) ->
  if messages.length == 0
    return ''
  messageLength = messages[0].length
  count = new Array(messageLength)
  for i in [0...messageLength]
    count[i] = {}
  for message in messages
    for j in [0...messageLength]
      char = message[j]
      count[j][char]?=0
      count[j][char]++
  originalMessage = ''
  for charCount in count
    minChar = null
    minCount = Number.MAX_SAFE_INTEGER
    for char, cnt of charCount
      if cnt < minCount
        minCount = cnt
        minChar = char
    originalMessage += minChar
  return originalMessage

readInputFile (messages) ->
  console.log getOriginalMessage(messages)
