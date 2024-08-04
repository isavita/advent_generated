fs = require 'fs'

Size = 10007

dealIntoNewStack = (deck) ->
  for i in [0...Size/2]
    [deck[i], deck[Size-i-1]] = [deck[Size-i-1], deck[i]]
  deck

cutN = (deck, n) ->
  if n >= 0
    deck[n...].concat(deck[...n])
  else
    deck[Size+n...].concat(deck[...Size+n])

dealWithIncrement = (deck, n) ->
  newDeck = new Array(Size)
  for i in [0...Size]
    newDeck[(i*n)%Size] = deck[i]
  newDeck

find2019 = (deck) ->
  for i in [0...Size]
    return i if deck[i] is 2019
  -1

deck = (i for i in [0...Size])

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  for line in data.split('\n')
    if line is 'deal into new stack'
      deck = dealIntoNewStack deck
    else if line.startsWith 'cut'
      n = parseInt line.split(' ')[1]
      deck = cutN deck, n
    else if line.startsWith 'deal with increment'
      n = parseInt line.split(' ').pop()
      deck = dealWithIncrement deck, n
  console.log find2019 deck