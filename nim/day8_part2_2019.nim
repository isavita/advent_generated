
import strutils, sequtils, os

let data = readFile("input.txt").strip()
let width = 25
let height = 6
let layerSize = width * height
var finalImage = newSeq[char](layerSize)
for i in 0..<layerSize:
  finalImage[i] = '2'

for i in countup(0, data.len - 1, layerSize):
  let layer = data[i ..< min(i + layerSize, data.len)]
  for j, pixel in layer:
    if finalImage[j] == '2':
      finalImage[j] = pixel

echo "Decoded image:"
for i in 0..<height:
  for j in 0..<width:
    let pixel = finalImage[i * width + j]
    if pixel == '0':
      stdout.write " "
    elif pixel == '1':
      stdout.write "#"
  echo ""
