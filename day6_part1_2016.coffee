
fs = require('fs')

readInputFile = (callback) ->
  fs.readFile 'input.txt', 'utf8', (err, data) ->
    if err
      console.error(err)
      return
    callback data.split('\n')

getCorrectedMessage = (messages) ->
  messageLength = messages[0].length
  count = new Array(messageLength)
  for i in [0...messageLength]
    count[i] = {}

  for message in messages
    for j in [0...messageLength]
      char = message[j]
      count[j][char] = (count[j][char] or 0) + 1

  correctedMessage = ''
  for charCount in count
    correctedMessage += getMostCommonChar(charCount)

  return correctedMessage

getMostCommonChar = (count) ->
  maxChar = null
  maxCount = 0
  for char, cnt of count
    if cnt > maxCount
      maxChar = char
      maxCount = cnt

  return maxChar

readInputFile (messages) ->
  console.log getCorrectedMessage(messages)
