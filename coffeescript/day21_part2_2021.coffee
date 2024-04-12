fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  input = data.trim()
  result = solve input
  console.log result

solve = (input) ->
  positions = parseInput input
  [w1, w2] = play [positions[0], positions[1]], [0, 0], 3, true, {}
  Math.max w1, w2

play = (positions, scores, rollsLeftInTurn, isPlayer1sTurn, memo) ->
  key = JSON.stringify {positions, scores, rollsLeftInTurn, isPlayer1sTurn}
  return memo[key] if memo[key]

  playerIndex = if isPlayer1sTurn then 0 else 1
  scoresCopy = scores.slice()

  if rollsLeftInTurn == 0
    scoresCopy[playerIndex] += positions[playerIndex]
    if scoresCopy[playerIndex] >= 21
      return [1, 0] if playerIndex == 0
      return [0, 1]

    isPlayer1sTurn = not isPlayer1sTurn
    rollsLeftInTurn = 3
    playerIndex = (playerIndex + 1) % 2

  wins1 = 0
  wins2 = 0
  for roll in [1..3]
    positionsCopy = positions.slice()
    positionsCopy[playerIndex] += roll
    positionsCopy[playerIndex] -= 10 if positionsCopy[playerIndex] > 10
    [r1, r2] = play positionsCopy, scoresCopy, rollsLeftInTurn - 1, isPlayer1sTurn, memo
    wins1 += r1
    wins2 += r2

  memo[key] = [wins1, wins2]
  [wins1, wins2]

parseInput = (input) ->
  ans = []
  for line in input.split '\n'
    parts = line.split ': '
    ans.push parseInt parts[1]
  ans