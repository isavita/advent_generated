
fs = require 'fs'
INF = 0x3f3f3f3f
MAX_BUTTONS = 50
MAX_COUNTERS = 20

buttons = Array(MAX_BUTTONS).fill().map -> []
btnSize = new Array(MAX_BUTTONS).fill 0
targets = new Array(MAX_COUNTERS).fill 0
matrixA = Array(MAX_COUNTERS).fill().map -> new Array(MAX_BUTTONS+1).fill 0
pivotCol = new Array(MAX_COUNTERS).fill -1
isPivot = new Array(MAX_BUTTONS).fill 0
pivotRows = new Array(MAX_BUTTONS).fill -1
freeVars = new Array(MAX_BUTTONS).fill 0
maxPresses = new Array(MAX_BUTTONS).fill INF
freeValues = new Array(MAX_BUTTONS).fill 0

numCounters = numButtons = rank = numFree = bestResult = 0

parseLine = (line) ->
  numCounters = numButtons = 0
  i = 0
  while i < line.length
    ch = line[i]
    if ch is '('
      i++; btnIdx = numButtons++; btnSize[btnIdx] = 0; buttons[btnIdx] = []
      while i < line.length and line[i] isnt ')'
        num = 0
        while i < line.length and line[i] >= '0' and line[i] <= '9'
          num = num*10 + (line.charCodeAt(i)-48); i++
        buttons[btnIdx].push num; btnSize[btnIdx]++
        if i < line.length and line[i] is ',' then i++
      i++ if i < line.length and line[i] is ')'
    else if ch is '{'
      i++; while i < line.length and line[i] isnt '}'
        num = 0
        while i < line.length and line[i] >= '0' and line[i] <= '9'
          num = num*10 + (line.charCodeAt(i)-48); i++
        targets[numCounters++] = num
        if i < line.length and line[i] is ',' then i++
      break
    else i++

gauss = ->
  for j in [0...numCounters]
    for k in [0..numButtons] then matrixA[j][k] = 0
    matrixA[j][numButtons] = targets[j]
  for i in [0...numButtons]
    for c in buttons[i]
      if c < numCounters then matrixA[c][i] = 1
  for i in [0...numCounters] then pivotCol[i] = -1
  row = 0
  for col in [0...numButtons] when row < numCounters
    maxRow = row
    for r in [row+1...numCounters]
      if Math.abs(matrixA[r][col]) > Math.abs(matrixA[maxRow][col]) then maxRow = r
    continue if Math.abs(matrixA[maxRow][col]) < 1e-9
    [matrixA[row], matrixA[maxRow]] = [matrixA[maxRow], matrixA[row]]
    scale = matrixA[row][col]
    for c in [col..numButtons] then matrixA[row][c] /= scale
    for r in [0...numCounters] when r != row and Math.abs(matrixA[r][col]) > 1e-9
      factor = matrixA[r][col]
      for c in [col..numButtons] then matrixA[r][c] -= factor*matrixA[row][c]
    pivotCol[row] = col
    row++
  rank = row
  for i in [0...numButtons] then isPivot[i]=0; pivotRows[i]=-1
  for r in [0...rank]
    c = pivotCol[r]
    if c >= 0 then isPivot[c]=1; pivotRows[c]=r
  numFree = 0
  for i in [0...numButtons] when !isPivot[i]
    freeVars[numFree++] = i
  for i in [0...numButtons]
    maxPresses[i] = INF
    for c in buttons[i] when c < numCounters and targets[c] < maxPresses[i]
      maxPresses[i] = targets[c]
    maxPresses[i] = 0 if maxPresses[i] is INF
  freeVars[0...numFree].sort (a,b) -> maxPresses[a]-maxPresses[b]

computePivots = (presses) ->
  for i in [0...numButtons] then presses[i]=0
  for i in [0...numFree] then presses[freeVars[i]] = freeValues[i]
  for r in [numCounters-1..0] by -1
    col = pivotCol[r]
    continue if col < 0
    val = matrixA[r][numButtons]
    for c in [col+1...numButtons] then val -= matrixA[r][c]*presses[c]
    intVal = Math.round(val)
    return 0 if Math.abs(val-intVal) > 1e-6 or intVal < 0 or intVal > maxPresses[col]
    presses[col] = intVal
  sum = 0; sum += presses[i] for i in [0...numButtons]
  sum

enumerate = (idx, curSum) ->
  return if curSum >= bestResult
  if idx is numFree
    presses = new Array(numButtons)
    sum = computePivots presses
    bestResult = sum if sum and sum < bestResult
    return
  fv = freeVars[idx]
  maxVal = maxPresses[fv]
  for v in [0..maxVal]
    freeValues[idx] = v
    enumerate idx+1, curSum+v

solve = ->
  gauss()
  bestResult = INF
  enumerate 0,0
  if bestResult is INF then -1 else bestResult

total = 0
for line in fs.readFileSync('input.txt','utf8').split('\n')
  continue if line.trim() is ''
  parseLine line
  res = solve()
  total += res if res > 0
console.log total
