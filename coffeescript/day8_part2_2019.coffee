
fs = require 'fs'

imageData = fs.readFileSync('input.txt', 'utf8').trim()
width = 25
height = 6
layerSize = width * height
finalImage = '2'.repeat(layerSize).split ''

for i in [0...imageData.length] by layerSize
  layer = imageData.slice i, i + layerSize
  for j, pixel of layer
    if finalImage[j] is '2'
      finalImage[j] = pixel

console.log "Decoded image:"
for i in [0...height]
  row = ''
  for j in [0...width]
    pixel = finalImage[i * width + j]
    row += if pixel is '0' then ' ' else if pixel is '1' then '#' else ''
  console.log row
