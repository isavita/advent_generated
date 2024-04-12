fs = require 'fs'

class Marble
  constructor: (@value, @prev = null, @next = null) ->
    @prev = @ if not @prev?
    @next = @ if not @next?

readInput = (filename) ->
  data = fs.readFileSync filename, 'utf8'
  parts = data.trim().split /\s+/
  players = parseInt parts[0]
  lastMarble = parseInt parts[6]
  [players, lastMarble]

playMarbleGame = (players, lastMarble) ->
  scores = new Array(players).fill 0
  current = new Marble 0
  for marble in [1..lastMarble]
    if marble % 23 == 0
      player = marble % players
      for i in [1..7]
        current = current.prev
      scores[player] += marble + current.value
      current.prev.next = current.next
      current.next.prev = current.prev
      current = current.next
    else
      current = current.next
      newMarble = new Marble marble, current, current.next
      current.next.prev = newMarble
      current.next = newMarble
      current = newMarble

  Math.max.apply Math, scores

[players, lastMarble] = readInput 'input.txt'
console.log playMarbleGame players, lastMarble