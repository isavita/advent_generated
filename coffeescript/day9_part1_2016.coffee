
fs = require('fs')
regex = /\((\d+)x(\d+)\)/

getDecompressedLength = (input) ->
  length = 0
  i = 0
  while i < input.length
    match = regex.exec input[i...]
    if match
      charCount = parseInt match[1]
      repeatCount = parseInt match[2]
      length += charCount * repeatCount
      i += match[0].length + charCount
    else
      length++
      i++
  return length

input = fs.readFileSync('input.txt', 'utf8').trim()
decompressedLength = getDecompressedLength(input)
console.log decompressedLength
