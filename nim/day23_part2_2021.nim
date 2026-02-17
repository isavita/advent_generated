
import heapqueue, tables, strutils

type State = object
  cost: int
  grid: string

proc `<`(a, b: State): bool = a.cost < b.cost

proc solve() =
  let data = readFile("input.txt").strip(trailing = true)
  if data == "": return
  let lines = data.splitLines()
  
  let r1 = [lines[2][3], lines[2][5], lines[2][7], lines[2][9]]
  let r2 = ['D', 'C', 'B', 'A']
  let r3 = ['D', 'B', 'A', 'C']
  let r4 = [lines[3][3], lines[3][5], lines[3][7], lines[3][9]]

  var startGrid = "..........."
  for i in 0..3:
    startGrid.add(r1[i])
    startGrid.add(r2[i])
    startGrid.add(r3[i])
    startGrid.add(r4[i])

  const
    target = "...........AAAABBBBCCCCDDDD"
    costs = [1, 10, 100, 1000]
    roomCols = [2, 4, 6, 8]
    hallwayIndices = [0, 1, 3, 5, 7, 9, 10]

  var pq = initHeapQueue[State]()
  pq.push(State(cost: 0, grid: startGrid))
  var dist = {startGrid: 0}.toTable

  while pq.len > 0:
    let curr = pq.pop()
    if curr.cost > dist.getOrDefault(curr.grid, high(int)): continue
    if curr.grid == target:
      echo curr.cost
      return

    for h in 0..10:
      let c = curr.grid[h]
      if c == '.': continue
      let charIdx = ord(c) - ord('A')
      let rCol = roomCols[charIdx]
      let rStart = 11 + charIdx * 4
      
      var clear = true
      let step = if rCol > h: 1 else: -1
      var currH = h + step
      while true:
        if curr.grid[currH] != '.':
          clear = false; break
        if currH == rCol: break
        currH += step
      if not clear: continue

      var targetSlot = -1
      for slot in 0..3:
        let occupant = curr.grid[rStart + slot]
        if occupant == '.': targetSlot = slot
        elif occupant != c: targetSlot = -1; break
      
      if targetSlot != -1:
        let steps = abs(rCol - h) + (targetSlot + 1)
        let newCost = curr.cost + steps * costs[charIdx]
        var nextGrid = curr.grid
        nextGrid[rStart + targetSlot] = c
        nextGrid[h] = '.'
        if newCost < dist.getOrDefault(nextGrid, high(int)):
          dist[nextGrid] = newCost
          pq.push(State(cost: newCost, grid: nextGrid))

    for rIdx in 0..3:
      let rStart = 11 + rIdx * 4
      let rCol = roomCols[rIdx]
      var slotToMove = -1
      for slot in 0..3:
        let c = curr.grid[rStart + slot]
        if c != '.':
          var needsMoving = false
          for subSlot in slot..3:
            if curr.grid[rStart + subSlot] != chr(ord('A') + rIdx):
              needsMoving = true; break
          if needsMoving: slotToMove = slot
          break
      
      if slotToMove == -1: continue
      let c = curr.grid[rStart + slotToMove]
      let charIdx = ord(c) - ord('A')
      for h in hallwayIndices:
        var clear = true
        let step = if h > rCol: 1 else: -1
        var currH = rCol
        while true:
          if curr.grid[currH] != '.':
            clear = false; break
          if currH == h: break
          currH += step
        if not clear: continue

        let steps = abs(h - rCol) + (slotToMove + 1)
        let newCost = curr.cost + steps * costs[charIdx]
        var nextGrid = curr.grid
        nextGrid[h] = c
        nextGrid[rStart + slotToMove] = '.'
        if newCost < dist.getOrDefault(nextGrid, high(int)):
          dist[nextGrid] = newCost
          pq.push(State(cost: newCost, grid: nextGrid))

solve()
