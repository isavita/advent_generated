
import std/[strutils, sequtils, math, tables]

const
  MAX_COUNTERS = 20
  MAX_BUTTONS = 50
  INF = 0x3f3f3f3f

type
  Button = array[MAX_COUNTERS, int]
  Buttons = array[MAX_BUTTONS, Button]
  IntArray = array[MAX_COUNTERS, int]
  DoubleArray = array[MAX_BUTTONS + 1, float]
  Matrix = array[MAX_COUNTERS, DoubleArray]
  IntArray50 = array[MAX_BUTTONS, int]

var
  buttons: Buttons
  btnSize: array[MAX_BUTTONS, int]
  targets: IntArray
  numCounters, numButtons: int
  matrix: Matrix
  pivotCol: IntArray
  isPivot, maxPresses: IntArray50
  pivotRows, freeVars: IntArray50
  numFree: int
  freeValues: IntArray50
  bestResult: int

proc parseLine(line: string) =
  numCounters = 0
  numButtons = 0
  var i = 0
  while i < line.len:
    if line[i] == '(':
      inc i
      let btnIdx = numButtons
      inc numButtons
      btnSize[btnIdx] = 0
      while i < line.len and line[i] != ')':
        var x = 0
        while i < line.len and line[i] in {'0'..'9'}:
          x = x * 10 + ord(line[i]) - ord('0')
          inc i
        buttons[btnIdx][btnSize[btnIdx]] = x
        inc btnSize[btnIdx]
        if i < line.len and line[i] == ',': inc i
      if i < line.len and line[i] == ')': inc i
    elif line[i] == '{':
      inc i
      while i < line.len and line[i] != '}':
        var x = 0
        while i < line.len and line[i] in {'0'..'9'}:
          x = x * 10 + ord(line[i]) - ord('0')
          inc i
        targets[numCounters] = x
        inc numCounters
        if i < line.len and line[i] == ',': inc i
      break
    else:
      inc i

proc gauss() =
  for j in 0..<numCounters:
    for i in 0..numButtons: matrix[j][i] = 0.0
    matrix[j][numButtons] = targets[j].float
  for i in 0..<numButtons:
    for j in 0..<btnSize[i]:
      let c = buttons[i][j]
      if c < numCounters: matrix[c][i] = 1.0
  for i in 0..<numCounters: pivotCol[i] = -1
  var row = 0
  for col in 0..<numButtons:
    if row >= numCounters: break
    var maxRow = row
    for r in (row + 1)..<numCounters:
      if abs(matrix[r][col]) > abs(matrix[maxRow][col]): maxRow = r
    if abs(matrix[maxRow][col]) < 1e-9: continue
    for c in 0..numButtons:
      swap(matrix[row][c], matrix[maxRow][c])
    let scale = matrix[row][col]
    for c in col..numButtons: matrix[row][c] /= scale
    for r in 0..<numCounters:
      if r != row and abs(matrix[r][col]) > 1e-9:
        let factor = matrix[r][col]
        for c in col..numButtons:
          matrix[r][c] -= factor * matrix[row][c]
    pivotCol[row] = col
    inc row
  let rank = row
  for i in 0..<numButtons:
    isPivot[i] = 0
    pivotRows[i] = -1
  for r in 0..<rank:
    let c = pivotCol[r]
    if c >= 0:
      isPivot[c] = 1
      pivotRows[c] = r
  numFree = 0
  for i in 0..<numButtons:
    if isPivot[i] == 0:
      freeVars[numFree] = i
      inc numFree
  for i in 0..<numButtons:
    maxPresses[i] = INF
    for j in 0..<btnSize[i]:
      let c = buttons[i][j]
      if c < numCounters and targets[c] < maxPresses[i]:
        maxPresses[i] = targets[c]
    if maxPresses[i] == INF: maxPresses[i] = 0
  for i in 0..<numFree:
    for j in (i + 1)..<numFree:
      if maxPresses[freeVars[i]] > maxPresses[freeVars[j]]:
        swap(freeVars[i], freeVars[j])

proc computePivots(presses: var IntArray50): int =
  for i in 0..<numButtons: presses[i] = 0
  for i in 0..<numFree: presses[freeVars[i]] = freeValues[i]
  for r in countdown(numCounters - 1, 0):
    let col = pivotCol[r]
    if col < 0: continue
    var val = matrix[r][numButtons]
    for c in (col + 1)..<numButtons:
      val -= matrix[r][c] * presses[c].float
    let intVal = int(round(val))
    if abs(val - intVal.float) > 1e-6: return 0
    if intVal < 0: return 0
    if intVal > maxPresses[col]: return 0
    presses[col] = intVal
  result = 0
  for i in 0..<numButtons: result += presses[i]

proc enumerate(idx, currentSum: int) =
  if currentSum >= bestResult: return
  if idx == numFree:
    var presses: IntArray50
    let sum = computePivots(presses)
    if sum > 0 and sum < bestResult: bestResult = sum
    return
  let fv = freeVars[idx]
  let maxVal = maxPresses[fv]
  for v in 0..maxVal:
    freeValues[idx] = v
    enumerate(idx + 1, currentSum + v)

proc solve(): int =
  gauss()
  bestResult = INF
  enumerate(0, 0)
  result = if bestResult == INF: -1 else: bestResult

when isMainModule:
  var total = 0
  for line in lines("input.txt"):
    if line.len == 0: continue
    parseLine(line)
    let res = solve()
    if res > 0: total += res
  echo total
