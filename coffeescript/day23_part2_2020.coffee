fs = require 'fs'

totalCups = 1000000
totalMoves = 10000000

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  cups = new Array(totalCups + 1)
  lastCup = undefined

  for i in [0...data.length]
    cup = parseInt(data[i])
    cups[lastCup] = cup if i > 0
    lastCup = cup

  for i in [data.length + 1..totalCups]
    cups[lastCup] = i
    lastCup = i

  cups[lastCup] = parseInt(data[0])

  currentCup = parseInt(data[0])
  for i in [0...totalMoves]
    pickup1 = cups[currentCup]
    pickup2 = cups[pickup1]
    pickup3 = cups[pickup2]

    cups[currentCup] = cups[pickup3]

    destinationCup = currentCup - 1
    destinationCup = totalCups if destinationCup == 0
    while destinationCup == pickup1 or destinationCup == pickup2 or destinationCup == pickup3
      destinationCup--
      destinationCup = totalCups if destinationCup == 0

    cups[pickup3] = cups[destinationCup]
    cups[destinationCup] = pickup1

    currentCup = cups[currentCup]

  cup1 = cups[1]
  cup2 = cups[cup1]
  console.log cup1 * cup2