fs = require 'fs'

readInput = (filename) ->
  data = fs.readFileSync(filename, 'utf8')
  lines = data.split '\n'
  algorithm = lines[0].replace /\r/g, ''
  image = []
  for line in lines[2..] when line.trim() isnt ''
    image.push line.trim().split ''
  [algorithm, image]

enhanceImage = (image, algorithm, times) ->
  for i in [0...times]
    flip = i % 2 is 1 and algorithm[0] is '#'
    image = applyAlgorithm(image, algorithm, flip)
  image

applyAlgorithm = (image, algorithm, flip) ->
  enhancedImage = []
  for i in [0...image.length + 2]
    enhancedImage.push []
    for j in [0...image[0].length + 2]
      index = calculateIndex(i - 1, j - 1, image, flip)
      enhancedImage[i].push algorithm[index]
  enhancedImage

calculateIndex = (i, j, image, flip) ->
  index = 0
  for di in [-1..1]
    for dj in [-1..1]
      index <<= 1
      if i + di >= 0 and i + di < image.length and j + dj >= 0 and j + dj < image[0].length
        if image[i + di][j + dj] is '#'
          index |= 1
      else if flip
        index |= 1
  index

countLitPixels = (image) ->
  count = 0
  for row in image
    for pixel in row
      count++ if pixel is '#'
  count

main = ->
  [algorithm, image] = readInput "input.txt"
  image = enhanceImage(image, algorithm, 2)
  console.log countLitPixels(image)

main()