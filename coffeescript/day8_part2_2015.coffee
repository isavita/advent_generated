
fs = require('fs')
readline = require('readline')

totalDiff = 0

inputStream = fs.createReadStream('input.txt')
inputReader = readline.createInterface({input: inputStream})

inputReader.on('line', (line) ->
  originalLength = line.length
  encodedLength = calculateEncodedLength(line)
  totalDiff += encodedLength - originalLength
)

inputReader.on('close', () ->
  console.log totalDiff
)

calculateEncodedLength = (s) ->
  encoded = '"'
  for ch in s
    if ch is '\\' or ch is '"'
      encoded += '\\'
    encoded += ch
  encoded += '"'
  encoded.length
