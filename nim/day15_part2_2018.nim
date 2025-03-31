
import sequtils, strutils, algorithm, tables, sets, deques, math, os

type
  Pos = tuple[r, c: int]
  UnitKind = enum Elf, Goblin
  Unit = ref object
    id: int # Unique ID for stable sorting if needed, also helps debugging
    pos: Pos
    kind: UnitKind
    hp: int
    ap: int
    alive: bool

# Reading order sort for positions
proc cmpPos(a, b: Pos): int =
  if a.r != b.r:
    result = cmp(a.r, b.r)
  else:
    result = cmp(a.c, b.c)

# Reading order sort for units
proc cmpUnitPos(a, b: Unit): int =
  cmpPos(a.pos, b.pos)

# Manhattan distance (not used for pathfinding, but useful conceptual check)
# proc dist(p1, p2: Pos): int =
#   abs(p1.r - p2.r) + abs(p1.c - p2.c)

const
  Wall = '#'
  Open = '.'
  InitialHP = 200
  GoblinAP = 3

let Dirs: array[4, Pos] = [(r: -1, c: 0), (r: 0, c: -1), (r: 0, c: 1), (r: 1, c: 0)] # Reading order for steps

type
  SimulationState = object
    grid: seq[string]
    units: seq[Unit]
    dims: Pos # rows, cols

proc deepCopyState(state: SimulationState): SimulationState =
  result.grid = state.grid # Grid itself is immutable enough for copy
  result.dims = state.dims
  result.units = newSeq[Unit](state.units.len)
  for i, u in state.units:
    result.units[i] = Unit(
        id: u.id, pos: u.pos, kind: u.kind, hp: u.hp, ap: u.ap, alive: u.alive
    )

# Finds the shortest path using BFS, considering tie-breaking rules
# Returns: tuple[foundPath: bool, nextStep: Pos]
proc findMove(state: var SimulationState, unit: Unit, targets: seq[Unit]): tuple[foundPath: bool, nextStep: Pos] =
  let (rows, cols) = state.dims
  var grid = state.grid # Local copy for marking occupied spaces

  # Mark occupied spaces temporarily
  for u in state.units:
    if u.alive and u != unit:
      grid[u.pos.r][u.pos.c] = 'X' # Mark occupied

  # 1. Identify target squares (adjacent to enemies)
  var targetSquares: HashSet[Pos]
  for target in targets:
    for dr in Dirs:
      let nr = target.pos.r + dr.r
      let nc = target.pos.c + dr.c
      if nr >= 0 and nr < rows and nc >= 0 and nc < cols and grid[nr][nc] == Open:
        targetSquares.incl((nr, nc))

  if targetSquares.len == 0:
    return (false, unit.pos) # No open squares next to targets

  # 2. BFS to find nearest reachable target squares
  var q = initDeque[tuple[pos: Pos, dist: int, firstStep: Pos]]()
  var visited = initTable[Pos, tuple[dist: int, firstStep: Pos]]()

  # Initial steps from unit's current position
  for dr in Dirs:
    let nextPos = (r: unit.pos.r + dr.r, c: unit.pos.c + dr.c)
    if nextPos.r >= 0 and nextPos.r < rows and nextPos.c >= 0 and nextPos.c < cols and grid[nextPos.r][nextPos.c] == Open:
      q.addLast((pos: nextPos, dist: 1, firstStep: nextPos))
      visited[nextPos] = (dist: 1, firstStep: nextPos)

  var reachableTargets: seq[tuple[pos: Pos, dist: int, firstStep: Pos]]
  var minDist = int.high

  while q.len > 0:
    let current = q.popFirst()

    # Optimization: If current path is already longer than best found path to a target, prune
    if current.dist > minDist: continue

    if current.pos in targetSquares:
      if current.dist < minDist:
        minDist = current.dist
        reachableTargets = @[(pos: current.pos, dist: current.dist, firstStep: current.firstStep)]
      elif current.dist == minDist:
        reachableTargets.add((pos: current.pos, dist: current.dist, firstStep: current.firstStep))
      # Continue searching for other paths of the same minimal distance

    # Explore neighbors
    for dr in Dirs:
      let nextPos = (r: current.pos.r + dr.r, c: current.pos.c + dr.c)
      if nextPos.r >= 0 and nextPos.r < rows and nextPos.c >= 0 and nextPos.c < cols and grid[nextPos.r][nextPos.c] == Open:
        if not visited.contains(nextPos) or visited[nextPos].dist > current.dist + 1:
           # Optimization: only add if shorter path found
           # We need all paths of minimal length though... let's rethink.
           # If we find a target, we note its distance. We only need to explore paths
           # up to that distance + 1 maybe?
           # Let's stick to standard BFS, finding all reachable targets first.

           # If not visited, or found a path of the same length *but possibly better first step*?
           # No, BFS guarantees shortest path first. We only need to record the *first* time
           # we reach a square with the minimal distance.
           if not visited.contains(nextPos):
             visited[nextPos] = (dist: current.dist + 1, firstStep: current.firstStep)
             q.addLast((pos: nextPos, dist: current.dist + 1, firstStep: current.firstStep))


  if reachableTargets.len == 0:
    return (false, unit.pos) # Cannot reach any target square

  # 3. Select the best target square (min dist, then reading order)
  reachableTargets.sort(proc(a, b: auto): int =
    if a.dist != b.dist: return cmp(a.dist, b.dist)
    return cmpPos(a.pos, b.pos)
  )
  let chosenDest = reachableTargets[0].pos # Best destination square

  # 4. Select the best first step towards the chosen destination
  # Filter candidates that reach the chosen destination with min distance
  var bestSteps: seq[Pos]
  for rt in reachableTargets:
      if rt.pos == chosenDest and rt.dist == minDist:
          bestSteps.add(rt.firstStep)

  # Remove duplicates and sort steps by reading order
  bestSteps = bestSteps.deduplicate().sorted(cmpPos)

  return (true, bestSteps[0]) # Return the best first step


proc runSimulation(initialState: SimulationState, elfAP: int, failOnElfDeath: bool): tuple[outcome: int, elvesWon: bool, rounds: int] =

  var state = deepCopyState(initialState)
  let initialElfCount = state.units.filterIt(it.kind == Elf).len

  # Set Elf AP
  for unit in state.units:
    if unit.kind == Elf:
      unit.ap = elfAP

  var rounds = 0
  var combatEndedPrematurely = false

  block simulationLoop:
    while true:
      # Sort units by reading order for turn sequence
      state.units.sort(cmpUnitPos)

      var unitMovedOrAttackedInRound = false # Track if any action happens

      for i in 0 ..< state.units.len:
        let unit = state.units[i]
        if not unit.alive: continue

        # 1. Identify targets
        let targetKind = if unit.kind == Elf: Goblin else: Elf
        var targets = state.units.filterIt(it.kind == targetKind and it.alive)

        if targets.len == 0:
          combatEndedPrematurely = true
          break simulationLoop # Combat ends

        # 2. Check if already in range
        var adjacentTargets: seq[Unit]
        for dr in Dirs:
          let adjacentPos = (r: unit.pos.r + dr.r, c: unit.pos.c + dr.c)
          for target in targets:
            if target.pos == adjacentPos:
              adjacentTargets.add(target)

        var moved = false
        if adjacentTargets.len == 0: # Not in range, try to move
          let moveResult = findMove(state, unit, targets)
          if moveResult.foundPath:
            # Update grid and unit position
            state.grid[unit.pos.r][unit.pos.c] = Open
            unit.pos = moveResult.nextStep
            state.grid[unit.pos.r][unit.pos.c] = if unit.kind == Elf: 'E' else: 'G'
            moved = true
            unitMovedOrAttackedInRound = true

            # Re-check adjacent targets after moving
            adjacentTargets = @[] # Clear previous check
            for dr in Dirs:
              let adjacentPos = (r: unit.pos.r + dr.r, c: unit.pos.c + dr.c)
              for target in targets: # Need to check against original full target list
                if target.alive and target.pos == adjacentPos: # Ensure target still alive
                  adjacentTargets.add(target)
          # else: Cannot move, turn ends

        # 3. Attack phase (if in range initially or after moving)
        if adjacentTargets.len > 0:
          unitMovedOrAttackedInRound = true
          # Select target: lowest HP, then reading order
          adjacentTargets.sort(proc(a, b: Unit): int =
            if a.hp != b.hp: return cmp(a.hp, b.hp)
            return cmpUnitPos(a, b)
          )
          let victim = adjacentTargets[0]

          # Deal damage
          victim.hp -= unit.ap

          if victim.hp <= 0:
            victim.alive = false
            state.grid[victim.pos.r][victim.pos.c] = Open
            if victim.kind == Elf and failOnElfDeath:
              # Elf died, simulation fails for Part 2 condition
              return (0, false, rounds)

      # Check if combat ended *during* the round processing
      if combatEndedPrematurely:
          break simulationLoop

      # If no unit could move or attack the entire round, it might be a stalemate?
      # The problem description implies combat ends when no targets are found at turn start.
      # Let's trust that condition.

      # Increment round counter *only if* the round fully completed
      rounds += 1


  # Calculate final score
  let remainingHP = state.units.filterIt(it.alive).mapIt(it.hp).sum()
  let outcome = rounds * remainingHP

  # Determine winner
  let remainingElves = state.units.filterIt(it.alive and it.kind == Elf).len
  let remainingGoblins = state.units.filterIt(it.alive and it.kind == Goblin).len
  let elvesWon = remainingGoblins == 0 and remainingElves > 0
  # Special check for Part 2: ensure no elf loss if required
  if failOnElfDeath and remainingElves != initialElfCount:
      return (outcome, false, rounds) # Elves won but lost someone

  return (outcome, elvesWon, rounds)


proc main() =
  let filename = "input.txt"
  if not fileExists(filename):
    echo "Error: input.txt not found."
    quit(1)

  let inputLines = readFile(filename).strip().splitLines()

  var initialState: SimulationState
  initialState.grid = inputLines
  initialState.dims = (r: inputLines.len, c: inputLines[0].len)
  var unitIdCounter = 0

  for r, row in inputLines:
    for c, cell in row:
      var kind: UnitKind
      var ap: int
      case cell:
      of 'E':
        kind = Elf
        ap = 3 # Default for Part 1, will be overwritten in simulation calls
      of 'G':
        kind = Goblin
        ap = GoblinAP
      else:
        continue

      initialState.units.add(Unit(
          id: unitIdCounter, pos: (r, c), kind: kind, hp: InitialHP, ap: ap, alive: true
      ))
      inc unitIdCounter

  # --- Part 1 ---
  let part1Result = runSimulation(initialState, 3, false) # Standard Elf AP=3, don't fail on death
  echo "Part 1 Outcome: ", part1Result.outcome
  # echo "  Rounds: ", part1Result.rounds
  # echo "  Elves Won: ", part1Result.elvesWon

  # --- Part 2 ---
  var elfAP = 4 # Start checking from AP = 4
  while true:
    let part2Result = runSimulation(initialState, elfAP, true) # Use current elfAP, fail if any Elf dies
    if part2Result.elvesWon: # This check implies no elves died due to failOnElfDeath=true
        echo "Part 2 Outcome: ", part2Result.outcome
        # echo "  Winning Elf AP: ", elfAP
        # echo "  Rounds: ", part2Result.rounds
        break
    elfAP += 1
    if elfAP > 200: # Sanity break
        echo "Error: Elf AP search exceeded limit."
        break

when isMainModule:
  main()
