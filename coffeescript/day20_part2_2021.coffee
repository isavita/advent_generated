fs = require 'fs'

iterations = 50
expandBy = 1

enhanceImage = (algorithm, image, useInfiniteLit) ->
  newImage = Array(image.length + 2 * expandBy).fill().map -> Array(image[0].length + 2 * expandBy).fill(false)
  for y in [-1...image.length + 1]
    for x in [-1...image[0].length + 1]
      index = 0
      for dy in [-1..1]
        for dx in [-1..1]
          index <<= 1
          ny = y + dy
          nx = x + dx
          if ny >= 0 and ny < image.length and nx >= 0 and nx < image[0].length
            index |= 1 if image[ny][nx]
          else
            index |= 1 if useInfiniteLit
      newImage[y + 1][x + 1] = algorithm[index] is '#'
  newImage

countLitPixels = (image) ->
  count = 0
  for row in image
    count += (pixel for pixel in row when pixel).length
  count

readInput = (filename) ->
  data = fs.readFileSync(filename, 'utf8').trim().split '\n\n'
  algorithm = data[0]
  image = (line.split('').map((char) -> char is '#') for line in data[1].split '\n')
  [algorithm, image]

[algorithm, image] = readInput 'input.txt'
for i in [0...iterations]
  image = enhanceImage algorithm, image, i % 2 is 1 and algorithm[0] is '#'
console.log countLitPixels image