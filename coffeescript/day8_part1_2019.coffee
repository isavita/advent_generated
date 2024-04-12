fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  imageData = data.trim()

  width = 25
  height = 6
  layerSize = width * height

  minZeros = layerSize + 1
  result = 0

  for i in [0...imageData.length] by layerSize
    layer = imageData[i..Math.min(i+layerSize-1, imageData.length-1)]
    zeroCount = oneCount = twoCount = 0

    for pixel in layer
      switch pixel
        when '0' then zeroCount++
        when '1' then oneCount++
        when '2' then twoCount++

    if zeroCount < minZeros
      minZeros = zeroCount
      result = oneCount * twoCount

  console.log result