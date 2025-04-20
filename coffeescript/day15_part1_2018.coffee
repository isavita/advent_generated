
# Day 15: Beverage Bandits

fs = require 'fs'

# --- Constants ---
WALL = '#'
OPEN = '.'
GOBLIN = 'G'
ELF = 'E'
INITIAL_HP = 200
ATTACK_POWER = 3

# --- Helper Functions ---

# Sort coordinates/units by reading order (top-to-bottom, left-to-right)
readingOrderSort = (a, b) ->
  if a.y isnt b.y then a.y - b.y else a.x - b.x

# Get adjacent coordinates (up, left, right, down)
getNeighbors = (x, y) ->
  [
    { x: x, y: y - 1 }, # Up
    { x: x - 1, y: y }, # Left
    { x: x + 1, y: y }, # Right
    { x: x, y: y + 1 }  # Down
  ]

# --- Unit Class ---
class Unit
  constructor: (@id, @type, @x, @y, @attackPower = ATTACK_POWER, @hp = INITIAL_HP) ->
    @alive = true

  toString: -> "#{@type}(#{@hp}) @ (#{@x}, #{@y})"

# --- Simulation Logic ---

# Breadth-First Search to find shortest paths
# Returns reachable targets with distances and first steps
bfs = (startX, startY, grid, units) ->
  q = []
  visited = {} # Store { "x,y": { dist: d, firstStep: {x, y} } }

  # Initialize blocked squares (walls and units)
  for y in [0...grid.length]
    for x in [0...grid[0].length]
      if grid[y][x] is WALL
        visited["#{x},#{y}"] = { dist: Infinity }
  for unit in units
    if unit.alive and (unit.x isnt startX or unit.y isnt startY)
      visited["#{unit.x},#{unit.y}"] = { dist: Infinity }

  # Start BFS
  startKey = "#{startX},#{startY}"
  visited[startKey] = { dist: 0, firstStep: null } # No first step from origin
  q.push { x: startX, y: startY, dist: 0, firstStep: null }

  head = 0
  while head < q.length
    current = q[head++]
    # console.log "BFS: Visiting #{current.x},#{current.y} dist #{current.dist}"

    for neighbor in getNeighbors(current.x, current.y)
      nx = neighbor.x
      ny = neighbor.y
      nKey = "#{nx},#{ny}"

      # Check bounds and if already visited or blocked
      if ny >= 0 and ny < grid.length and nx >= 0 and nx < grid[0].length and not visited[nKey]?
        # Determine the first step
        firstStep = current.firstStep ? { x: nx, y: ny } # If origin, this is the first step

        visited[nKey] = { dist: current.dist + 1, firstStep: firstStep }
        q.push { x: nx, y: ny, dist: current.dist + 1, firstStep: firstStep }

  # Return the visited map containing distances and first steps
  visited


# Find the best move for a unit
findMove = (unit, targets, grid, units) ->
  # 1. Identify in-range squares adjacent to targets
  inRangeSquares = []
  targetPositions = new Set(targets.map (t) -> "#{t.x},#{t.y}")

  for target in targets
    for neighbor in getNeighbors(target.x, target.y)
      nx = neighbor.x
      ny = neighbor.y
      nKey = "#{nx},#{ny}"
      # Check bounds, if open, and not occupied by another unit (except the current one potentially)
      if ny >= 0 and ny < grid.length and nx >= 0 and nx < grid[0].length and
         grid[ny][nx] is OPEN and not units.some((u) -> u.alive and u.x is nx and u.y is ny)
        # Add unique squares
        if not inRangeSquares.some((sq) -> sq.x is nx and sq.y is ny)
            inRangeSquares.push { x: nx, y: ny }

  if inRangeSquares.length is 0
    return null # No path possible

  # Sort potential destinations by reading order
  inRangeSquares.sort(readingOrderSort)

  # 2. BFS from the unit's position
  pathData = bfs(unit.x, unit.y, grid, units)

  # 3. Find reachable in-range squares and their path info
  reachableDestinations = []
  for sq in inRangeSquares
    key = "#{sq.x},#{sq.y}"
    if pathData[key]?
      reachableDestinations.push {
        x: sq.x, y: sq.y,
        dist: pathData[key].dist,
        firstStep: pathData[key].firstStep
      }

  if reachableDestinations.length is 0
    return null # Cannot reach any in-range square

  # 4. Find the minimum distance
  minDist = Math.min(...reachableDestinations.map (d) -> d.dist)

  # 5. Filter by minimum distance
  nearestDestinations = reachableDestinations.filter (d) -> d.dist is minDist

  # 6. Choose the target square (already sorted by reading order)
  chosenDestination = nearestDestinations[0] # Already sorted

  # 7. Choose the best first step towards the chosen destination
  # We need the first step recorded during BFS for the chosen destination
  bestFirstStep = chosenDestination.firstStep

  # Filter potential first steps (neighbors of the unit) that lead to the chosen destination via a shortest path
  possibleFirstSteps = []
  for neighbor in getNeighbors(unit.x, unit.y)
    nx = neighbor.x
    ny = neighbor.y
    nKey = "#{nx},#{ny}"
    # Check if neighbor is valid, open, unoccupied, and part of a shortest path to chosen dest
    if pathData[nKey]? and pathData[nKey].dist is 1 and pathData[nKey].firstStep?
        # Further check: Does this step lead towards the chosen destination?
        # We actually rely on the BFS having stored the *correct* first step for the chosen destination.
        # So, we just need to find which immediate neighbor corresponds to that stored first step.
        if pathData[nKey].firstStep.x is bestFirstStep.x and pathData[nKey].firstStep.y is bestFirstStep.y
            possibleFirstSteps.push { x: nx, y: ny }


  if not bestFirstStep?
     # This case should theoretically not happen if a destination was chosen, but handle defensively
     # console.error "Error: Chosen destination but no first step found?"
     return null

  # There should only be one bestFirstStep matching the chosen destination's path data.
  # If multiple paths existed, BFS tie-breaking via neighbor order + reading order target selection handles it.
  # Return the determined best first step.
  return bestFirstStep


# Perform an attack if possible
performAttack = (unit, units, grid) ->
  targetsInRange = []
  enemyType = if unit.type is GOBLIN then ELF else GOBLIN

  for neighbor in getNeighbors(unit.x, unit.y)
    for targetUnit in units
      if targetUnit.alive and targetUnit.type is enemyType and
         targetUnit.x is neighbor.x and targetUnit.y is neighbor.y
        targetsInRange.push targetUnit

  if targetsInRange.length is 0
    return false # No attack performed

  # Find target with lowest HP, tie-break with reading order
  targetsInRange.sort (a, b) ->
    if a.hp isnt b.hp then a.hp - b.hp else readingOrderSort(a, b)

  targetToAttack = targetsInRange[0]

  # Deal damage
  targetToAttack.hp -= unit.attackPower

  # Check if target died
  if targetToAttack.hp <= 0
    targetToAttack.alive = false
    grid[targetToAttack.y][targetToAttack.x] = OPEN # Clear the grid square
    # console.log "#{unit.type} at (#{unit.x},#{unit.y}) killed #{targetToAttack.type} at (#{targetToAttack.x},#{targetToAttack.y})"


  return true # Attack performed


# Simulate one round of combat
simulateRound = (units, grid) ->
  # Sort units by reading order at the start of the round
  units.sort(readingOrderSort)
  combatEnded = false
  unitMovedOrAttacked = false # Track if any action happened in the round

  for unit in units
    if not unit.alive then continue # Skip dead units

    # 1. Identify Targets
    enemyType = if unit.type is GOBLIN then ELF else GOBLIN
    targets = units.filter (u) -> u.alive and u.type is enemyType

    # If no targets remain, combat ends immediately
    if targets.length is 0
      combatEnded = true
      break

    # 2. Check if already in range of a target
    inRange = false
    for neighbor in getNeighbors(unit.x, unit.y)
        if targets.some (t) -> t.x is neighbor.x and t.y is neighbor.y
            inRange = true
            break

    # 3. Move if not in range
    moved = false
    if not inRange
      moveTarget = findMove(unit, targets, grid, units)
      if moveTarget?
        # Update grid
        grid[unit.y][unit.x] = OPEN
        # Update unit position
        unit.x = moveTarget.x
        unit.y = moveTarget.y
        # Update grid with new position
        grid[unit.y][unit.x] = unit.type
        moved = true
        unitMovedOrAttacked = true
        # Re-check if now in range after moving
        for neighbor in getNeighbors(unit.x, unit.y)
            if targets.some (t) -> t.x is neighbor.x and t.y is neighbor.y
                inRange = true
                break

    # 4. Attack if in range (either initially or after moving)
    if inRange
      attacked = performAttack(unit, units, grid)
      if attacked then unitMovedOrAttacked = true


  return combatEnded # Return true if combat ended during this round

# --- Main Execution ---
main = ->
  try
    input = fs.readFileSync('input.txt', 'utf8')
  catch e
    console.error "Error reading input.txt: #{e.message}"
    process.exit(1)

  grid = input.trim().split('\n').map (line) -> line.split('')
  units = []
  unitIdCounter = 0

  # Parse initial state
  for y in [0...grid.length]
    for x in [0...grid[0].length]
      char = grid[y][x]
      if char is GOBLIN or char is ELF
        units.push new Unit(unitIdCounter++, char, x, y)

  rounds = 0
  # printGrid = (r) ->
  #   console.log "\nAfter #{r} rounds:"
  #   for row, y in grid
  #       unitInfo = units.filter((u) -> u.alive and u.y is y).sort(readingOrderSort).map((u) -> "#{u.type}(#{u.hp})").join(", ")
  #       console.log row.join('') + "   #{unitInfo}"
  #   console.log ""

  # printGrid("Initial")

  loop
    combatEnded = simulateRound(units, grid)
    if combatEnded
      break
    rounds += 1
    # printGrid(rounds)
    # if rounds > 50 then break # Safety break for debugging


  # Calculate final score
  remainingUnits = units.filter (u) -> u.alive
  totalRemainingHp = remainingUnits.reduce ((sum, u) -> sum + u.hp), 0

  outcome = rounds * totalRemainingHp

  console.log "Combat ends after #{rounds} full rounds"
  winner = if remainingUnits[0]?.type is ELF then "Elves" else "Goblins"
  console.log "#{winner} win with #{totalRemainingHp} total hit points left"
  console.log "Outcome: #{outcome}"

# Run the main function
main()
