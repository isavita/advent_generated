
import os, strutils, sequtils, tables

type
  Halves = object
    isChip: bool
    material: string

  State = object
    floors: seq[seq[Halves]]
    elevatorLevel, steps: int

proc newInitialState(input: string): State =
  var s: State
  s.floors = newSeq[seq[Halves]](4)

  for lineIndex, line in input.splitLines():
    for part in line.split(' '):
      let trimmed = part.strip(",.")
      if trimmed == "generator":
        s.floors[lineIndex].add(Halves(false, line.split(' ')[line.split(' ').findIndex(it == "generator") - 1]))
      elif trimmed == "microchip":
        s.floors[lineIndex].add(Halves(true, line.split(' ')[line.split(' ').findIndex(it == "microchip") - 1][0..^9]))

  return s

proc isValid(s: State): bool =
  for i in 0 ..< s.floors.len:
    var gensSeen = initTable[string, bool]()
    for half in s.floors[i]:
      if not half.isChip:
        gensSeen[half.material] = true
    if gensSeen.len > 0:
      for half in s.floors[i]:
        if half.isChip and not gensSeen.hasKey(half.material):
          return false
  return true

proc isDone(s: State): bool =
  return s.floors[0].len + s.floors[1].len + s.floors[2].len == 0

proc hashKey(s: State): string =
  var genChipPairs: seq[(int, int)] = @[]
  for flIndex, fl in s.floors:
    for half in fl:
      if half.isChip:
        genChipPairs.add((flIndex, -1))
      else:
        genChipPairs.add((-1, flIndex))
  return $s.elevatorLevel & genChipPairs.sorted()

proc getNextStates(s: State): seq[State] =
  var futureStates: seq[State] = @[]
  let currentLevel = s.floors[s.elevatorLevel]
  let eleDiffs = if s.elevatorLevel < 3: @[1] else: @[] & if s.elevatorLevel > 0: @[-1] else: @[]

  for eleDiff in eleDiffs:
    for i in 0 ..< currentLevel.len:
      for j in i + 1 ..< currentLevel.len:
        var cl = s
        cl.elevatorLevel += eleDiff
        cl.steps += 1
        cl.floors[cl.elevatorLevel].add(cl.floors[s.elevatorLevel][i])
        cl.floors[cl.elevatorLevel].add(cl.floors[s.elevatorLevel][j])
        cl.floors[s.elevatorLevel].delete(j)
        cl.floors[s.elevatorLevel].delete(i)
        if cl.isValid():
          futureStates.add(cl)
    for i in 0 ..< currentLevel.len:
      var cl = s
      cl.elevatorLevel += eleDiff
      cl.steps += 1
      cl.floors[cl.elevatorLevel].add(cl.floors[s.elevatorLevel][i])
      cl.floors[s.elevatorLevel].delete(i)
      if cl.isValid():
        futureStates.add(cl)

  return futureStates

proc rtgHellDay(input: string): int =
  var currentState = newInitialState(input)
  currentState.floors[0].add(Halves(false, "elerium"))
  currentState.floors[0].add(Halves(true, "elerium"))
  currentState.floors[0].add(Halves(false, "dilithium"))
  currentState.floors[0].add(Halves(true, "dilithium"))

  var queue = @[(currentState)]
  var prevStates = initTable[string, bool]()

  while queue.len > 0:
    let front = queue.pop()
    if front.isDone():
      return front.steps
    let hash = front.hashKey()
    if prevStates.hasKey(hash):
      continue
    prevStates[hash] = true
    let nextStates = front.getNextStates()
    queue.add(nextStates)

  return -1

proc readFile(pathFromCaller: string): string =
  let content = readFile(pathFromCaller)
  return content.strip()

echo rtgHellDay(readFile("input.txt"))
