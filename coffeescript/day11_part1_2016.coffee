
fs = require 'fs'

rtgHellDay = (inputStr, part) ->
  newInitialState = (input) ->
    s =
      floors: [[] for _ in [0...4]]
      elevatorLevel: 0
      steps: 0

    for line, lineIndex in input.split("\n")
      parts = (v.trim().replace(",.", "") for v in line.split(" "))
      for word, i in parts
        if word is "generator"
          material = parts[i - 1]
          s.floors[lineIndex].push
            isChip: false
            material: material
        else if word is "microchip"
          material = parts[i - 1].split("-comp")[0]
          s.floors[lineIndex].push
            isChip: true
            material: material

    s

  hashKey = (s) ->
    mapGenToIndex = {}
    mapChipToIndex = {}
    for fl, flIndex in s.floors
      for half in fl
        if half.isChip
          mapChipToIndex[half.material] = flIndex
        else
          mapGenToIndex[half.material] = flIndex

    genChipPairs = []
    for material of mapGenToIndex
      genChipPairs.push [mapGenToIndex[material], mapChipToIndex[material]]

    genChipPairs.sort()
    "#{s.elevatorLevel}#{genChipPairs}"

  isValid = (s) ->
    for fl, i in s.floors
      gensSeen = {}
      for half in fl
        gensSeen[half.material] = true unless half.isChip

      for half in fl
        return false if half.isChip and not gensSeen[half.material]

    true

  isDone = (s) ->
    lenSum = (fl.length for fl in s.floors[0...3]).reduce (a, b) -> a + b
    lenSum is 0

  getMovablePermIndices = (s) ->
    permsToMove = []
    currentLevel = s.floors[s.elevatorLevel]

    for i in [0...currentLevel.length]
      for j in [i + 1...currentLevel.length]
        permsToMove.push [i, j]

    for i in [0...currentLevel.length]
      permsToMove.push [i]

    permsToMove

  clone = (s) ->
    cl =
      floors: (fl.slice() for fl in s.floors)
      elevatorLevel: s.elevatorLevel
      steps: s.steps
    cl

  getNextStates = (s) ->
    futureStates = []
    movablePermIndices = getMovablePermIndices(s)
    eleDiffs = if s.elevatorLevel < 3 then [1] else []
    eleDiffs = eleDiffs.concat [-1] if s.elevatorLevel > 0

    for eleDiff in eleDiffs
      for permIndices in movablePermIndices
        cl = clone(s)
        cl.elevatorLevel += eleDiff
        cl.steps += 1
        oldLevel = s.elevatorLevel
        newLevel = cl.elevatorLevel

        for index in permIndices
          cl.floors[newLevel].push s.floors[oldLevel][index]

        for in_ in [permIndices.length - 1..0]
          cl.floors[oldLevel][permIndices[in_]] = cl.floors[oldLevel].pop()

        futureStates.push cl if isValid(cl)

    futureStates

  currentState = newInitialState(inputStr)

  if part is 2
    currentState.floors[0] = currentState.floors[0].concat [
      {isChip: false, material: "elerium"}
      {isChip: true, material: "elerium"}
      {isChip: false, material: "dilithium"}
      {isChip: true, material: "dilithium"}
    ]

  queue = [currentState]
  prevStates = {}
  while queue.length > 0
    front = queue.shift()

    return front.steps if isDone(front)

    hash_key = hashKey(front)
    continue if prevStates[hash_key]
    prevStates[hash_key] = true

    nextStates = getNextStates(front)
    queue = queue.concat nextStates

  -1

inputData = fs.readFileSync("input.txt", "utf8")
part = 1
ans = rtgHellDay(inputData, part)
console.log ans
