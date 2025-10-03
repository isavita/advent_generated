
import strutils, algorithm, tables, sequtils, sets, deques, os, hashes

type
  Item = object
    isChip: bool
    material: string

  State = object
    floors: array[0..3, seq[Item]]
    elevatorLevel: int
    steps: int

proc cleanWord(w: string): string =
  w.strip(chars = {' ', ',', '.'})

proc newInitialState(input: string): State =
  var s: State
  for i in 0..3:
    s.floors[i] = @[]
  s.steps = 0
  s.elevatorLevel = 0

  let lines = input.splitLines()

  for lineIndex, line in lines:
    let parts = line.split()
    for i, word in parts:
      let cleanedWord = cleanWord(word)
      if cleanedWord == "generator":
        let material = cleanWord(parts[i-1])
        s.floors[lineIndex].add(Item(isChip: false, material: material))
      elif cleanedWord == "microchip":
        var materialRaw = cleanWord(parts[i-1])
        let suffixIndex = materialRaw.find("-comp")
        var material: string
        if suffixIndex >= 0:
          material = materialRaw[0..<suffixIndex]
        else:
          material = materialRaw
        s.floors[lineIndex].add(Item(isChip: true, material: material))
  return s

proc hashKey(s: State): string =
  var mapGenToIndex = initTable[string, int]()
  var mapChipToIndex = initTable[string, int]()

  for flIndex, fl in s.floors.pairs:
    for half in fl:
      if half.isChip:
        mapChipToIndex[half.material] = flIndex
      else:
        mapGenToIndex[half.material] = flIndex

  var genChipPairs: seq[tuple[genFl: int, chipFl: int]] = @[]

  for material, genFl in mapGenToIndex.pairs:
    if mapChipToIndex.contains(material):
      let chipFl = mapChipToIndex[material]
      genChipPairs.add((genFl, chipFl))

  genChipPairs.sort(cmp)

  var result = $s.elevatorLevel
  for pair in genChipPairs:
    result.add($pair.genFl)
    result.add($pair.chipFl)

  return result

proc isValid(s: State): bool =
  for fl in s.floors:
    var gensSeen = initHashSet[string]()
    
    for half in fl:
      if not half.isChip:
        gensSeen.incl(half.material)

    if gensSeen.len == 0:
      continue

    for half in fl:
      if half.isChip:
        if not gensSeen.contains(half.material):
          return false
  return true

proc isDone(s: State): bool =
  var lenSum = 0
  for i in 0..2:
    lenSum += s.floors[i].len
  return lenSum == 0

proc getMovablePermIndices(s: State): seq[seq[int]] =
  let currentLevel = s.floors[s.elevatorLevel]
  let N = currentLevel.len
  var permsToMove: seq[seq[int]] = @[]

  for i in 0..<N:
    for j in i+1..<N:
      permsToMove.add(@[i, j])

  for i in 0..<N:
    permsToMove.add(@[i])

  return permsToMove

proc clone(s: State): State =
  var cl: State
  cl.elevatorLevel = s.elevatorLevel
  cl.steps = s.steps
  
  for i in 0..3:
    cl.floors[i] = s.floors[i]
  return cl

proc getNextStates(s: State): seq[State] =
  var futureStates: seq[State] = @[]
  let movablePermIndices = getMovablePermIndices(s)
  
  var eleDiffs: seq[int] = @[]
  if s.elevatorLevel < 3: eleDiffs.add(1)
  if s.elevatorLevel > 0: eleDiffs.add(-1)

  for eleDiff in eleDiffs:
    for permIndices in movablePermIndices:
      var cl = clone(s)
      cl.elevatorLevel += eleDiff
      cl.steps += 1
      let oldLevel = s.elevatorLevel
      let newLevel = cl.elevatorLevel

      for index in permIndices:
        cl.floors[newLevel].add(s.floors[oldLevel][index])

      var indicesToRemove = permIndices
      indicesToRemove.sort(cmp, Descending)

      for idx in indicesToRemove:
        cl.floors[oldLevel].delete(idx)

      if isValid(cl):
        futureStates.add(cl)

  return futureStates

proc rtgHellDay(inputStr: string, part: int): int =
  var currentState = newInitialState(inputStr)

  if part == 2:
    currentState.floors[0].add(Item(isChip: false, material: "elerium"))
    currentState.floors[0].add(Item(isChip: true, material: "elerium"))
    currentState.floors[0].add(Item(isChip: false, material: "dilithium"))
    currentState.floors[0].add(Item(isChip: true, material: "dilithium"))

  var queue = initDeque[State]()
  queue.addLast(currentState)
  
  var prevStates = initHashSet[string]()

  while queue.len > 0:
    let front = queue.popFirst()

    if isDone(front):
      return front.steps

    let hashKey = hashKey(front)
    if prevStates.contains(hashKey):
      continue
    prevStates.incl(hashKey)

    let nextStates = getNextStates(front)
    for nextS in nextStates:
      queue.addLast(nextS)
      
  return -1

when isMainModule:
  let inputData = readFile("input.txt")
  
  let ans1 = rtgHellDay(inputData, 1)
  echo ans1
  
  # Since the Python solution implicitly runs both parts for the provided input structure:
  # let ans2 = rtgHellDay(inputData, 2)
  # echo ans2
