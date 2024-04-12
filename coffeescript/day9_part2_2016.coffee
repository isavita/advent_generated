fs = require 'fs'

getDecompressedLengthV2 = (input) ->
  decompress input, 0, input.length

decompress = (input, start, end) ->
  markerRegex = /\((\d+)x(\d+)\)/g
  length = 0
  i = start
  while i < end
    markerRegex.lastIndex = i
    match = markerRegex.exec input
    if match?
      charCount = parseInt match[1]
      repeatCount = parseInt match[2]
      nextIndex = markerRegex.lastIndex
      subSection = input.substring(nextIndex, nextIndex + charCount)
      expandedSectionLength = decompress(subSection, 0, subSection.length)
      length += repeatCount * expandedSectionLength
      i = nextIndex + charCount
    else
      length += end - i
      break
  length

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.error "Error reading file: #{err}"
    return
  try
    decompressedLength = getDecompressedLengthV2 data
    console.log "Decompressed Length: #{decompressedLength}"
  catch error
    console.error "Error processing data: #{error}"