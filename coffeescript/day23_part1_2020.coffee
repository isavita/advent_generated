fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  input = data.trim()
  cups = new Array(input.length + 1)
  currentCup = parseInt(input[0])

  for i in [0...input.length]
    cup = parseInt(input[i])
    if i < input.length - 1
      nextCup = parseInt(input[i + 1])
      cups[cup] = nextCup
    else
      cups[cup] = parseInt(input[0])

  for i in [0...100]
    pickup1 = cups[currentCup]
    pickup2 = cups[pickup1]
    pickup3 = cups[pickup2]
    cups[currentCup] = cups[pickup3]

    destinationCup = currentCup - 1
    destinationCup = input.length if destinationCup < 1
    while destinationCup == pickup1 or destinationCup == pickup2 or destinationCup == pickup3
      destinationCup -= 1
      destinationCup = input.length if destinationCup < 1

    cups[pickup3] = cups[destinationCup]
    cups[destinationCup] = pickup1
    currentCup = cups[currentCup]

  result = []
  cup = cups[1]
  while cup != 1
    result.push cup
    cup = cups[cup]
    break if cup == 1

  console.log result.join('')