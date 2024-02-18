
fs = require 'fs'

totalDiff = 0

readFile = (err, data) ->
  if err
    console.error "Error reading file: #{err}"
    return

  lines = data.toString().split('\n')

  for line in lines
    codeLength = line.length
    memoryLength = calculateMemoryLength(line)
    totalDiff += codeLength - memoryLength

  console.log totalDiff

calculateMemoryLength = (s) ->
  length = 0
  inEscape = false
  hexCount = 0

  for i in [1...s.length - 1]
    switch
      when hexCount > 0
        hexCount--
      when inEscape
        if s[i] == 'x'
          hexCount = 2
        inEscape = false
        length++
      when s[i] == '\\'
        inEscape = true
      else
        length++

  length

fs.readFile 'input.txt', 'utf8', readFile
