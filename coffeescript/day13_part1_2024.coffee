
fs = require 'fs'

parseVal = (s) ->
  s = s.trim().replace(/^X\+|Y\+|X=|Y=/, "")
  parseInt(s)

parseValPrize = (s) ->
  s = s.trim().replace(/^X=|Y=/, "")
  parseInt(s)

parseLine = (s) ->
  parts = s.trim().split(',')
  x = parseVal(parts[0])
  y = parseVal(parts[1])
  [x, y]

parsePrize = (s) ->
  parts = s.trim().split(',')
  x = parseValPrize(parts[0])
  y = parseValPrize(parts[1])
  [x, y]

parseMachine = (lines) ->
  m = {}
  for l in lines
    l = l.replace("Button A:", "A:").replace("Button B:", "B:").replace("Prize:", "P:")
    if l.startsWith("A:")
      [m.ax, m.ay] = parseLine(l[2..])
    else if l.startsWith("B:")
      [m.bx, m.by] = parseLine(l[2..])
    else if l.startsWith("P:")
      [m.px, m.py] = parsePrize(l[2..])
  m

solveMachine = (m) ->
  minCost = -1
  for aCount in [0..100]
    for bCount in [0..100]
      x = m.ax * aCount + m.bx * bCount
      y = m.ay * aCount + m.by * bCount
      if x is m.px and y is m.py
        cost = aCount * 3 + bCount
        if minCost is -1 or cost < minCost
          minCost = cost
  minCost

main = ->
    lines = fs.readFileSync("input.txt", "utf-8").trim().split('\n')
    machines = []
    currentMachine = []
    for line in lines
        line=line.trim()
        if not line
            if currentMachine.length > 0
                machines.push parseMachine(currentMachine)
            currentMachine = []
        else
            currentMachine.push line

    if currentMachine.length > 0
        machines.push parseMachine(currentMachine)

    results = (solveMachine(m) for m in machines when solveMachine(m) != -1)

    if results.length is 0
      console.log "0 0"
    else
      console.log results.length, results.reduce(((a, b) -> a + b), 0)
main()
