
# Day 15: Beverage Bandits - CoffeeScript Solution

fs = require 'fs'

# Directions in reading order: Up, Left, Right, Down
DIRS = [[0, -1], [-1, 0], [1, 0], [0, 1]]

# Helper to create unique key for coordinates
posKey = (y, x) -> "#{y},#{x}"

class Unit
  constructor: (@type, @x, @y, @ap = 3, @hp = 200, @alive = true) ->

  # Sort function for reading order
  readOrder: (other) -> @y - other.y || @x - other.x

  # Check if another unit is an enemy
  isEnemy: (other) -> @type isnt other.type

# BFS: Calculates shortest distances from startY, startX to all reachable '.' squares
# Considers other living units as obstacles. Returns a map of { "y,x": distance }.
bfsDistances = (grid, units, startY, startX) ->
  q = [{ y: startY, x: startX, dist: 0 }] # Queue stores {y, x, distance}
  distances = {}
  distances[posKey(startY, startX)] = 0 # Distance from start to start is 0
  height = grid.length
  width = grid[0].length
  head = 0 # Use index as queue pointer for efficiency

  # Create a set of positions occupied by living units for quick lookup
  livingUnitPositions = new Set()
  for unit in units
    livingUnitPositions.add(posKey(unit.y, unit.x)) if unit.alive


  while head < q.length
    { y, x, dist } = q[head++]

    for [dx, dy] in DIRS # Explore neighbors in reading order
      ny = y + dy
      nx = x + dx
      key = posKey(ny, nx)

      # Check bounds, if it's an open cavern '.',
      # if it's not occupied by a living unit (excluding start pos itself initially),
      # and if we haven't found a path to it yet.
      if ny >= 0 and ny < height and nx >= 0 and nx < width and \
         grid[ny][nx] is '.' and \
         not livingUnitPositions.has(key) and \
         not distances[key]?

        distances[key] = dist + 1
        q.push { y: ny, x: nx, dist: dist + 1 }

  return distances

# Simulates the entire combat process
# Takes initial grid (as array of char arrays), initial unit data, elf attack power,
# and a flag for part 2 (to enable early exit if an elf dies).
# Returns { outcome: number, elfDied: boolean }
simulateCombat = (initialGridLines, initialUnitData, elfAP = 3, part2 = false) ->
  # --- Initialization ---
  # Deep copy grid and create fresh Unit instances for this simulation
  grid = initialGridLines.map (line) -> line.split('')
  units = initialUnitData.map (u) -> new Unit(u.type, u.x, u.y, u.ap, u.hp, u.alive)

  # Set Elf attack power for this simulation run
  initialElfCount = 0
  for unit in units
    if unit.type is 'E'
      unit.ap = elfAP
      initialElfCount += 1

  roundsCompleted = 0
  elfDiedInCombat = false

  # --- Main Combat Loop ---
  loop
    # Order units by reading order at the start of each round
    units.sort (a, b) -> a.readOrder(b)
    roundFinished = true # Assume round completes unless combat ends

    # --- Unit Turn Loop ---
    for unit in units
      continue unless unit.alive # Skip dead units

      # 1. Identify Targets: Find all living enemies
      enemies = units.filter (other) -> other.alive and unit.isEnemy(other)

      # Check for End of Combat: If no enemies remain, combat ends
      if enemies.length is 0
        roundFinished = false # Combat ended before this unit's turn finished the round
        break # Exit the unit turn loop

      # 2. Check if Already In Range: Find enemies in adjacent squares
      targetsInRange = []
      for [dx, dy] in DIRS
        ny = unit.y + dy
        nx = unit.x + dx
        # Check if any living enemy is at this adjacent position
        for enemy in enemies
          if enemy.x is nx and enemy.y is ny
            targetsInRange.push enemy
            break # Found one in this direction, no need to check others for this spot

      # 3. Movement Phase (if not already in range)
      moved = false
      if targetsInRange.length is 0
        # 3a. Identify Potential Destination Squares: Open squares adjacent to any enemy
        targetSquares = []
        targetSquareSet = new Set() # To avoid duplicates
        for enemy in enemies
          for [dx, dy] in DIRS
            ty = enemy.y + dy
            tx = enemy.x + dx
            key = posKey(ty, tx)

            # Check bounds, must be open cavern '.', not occupied by ANY living unit, and not already added
            if ty >= 0 and ty < grid.length and tx >= 0 and tx < grid[0].length and \
               grid[ty][tx] is '.' and \
               not units.find((u) -> u.alive and u.y is ty and u.x is tx) and \
               not targetSquareSet.has(key)
              targetSquares.push { y: ty, x: tx }
              targetSquareSet.add(key)

        # Proceed only if there are potential places to move to
        if targetSquares.length > 0
          # 3b. Find Reachable Destinations: Use BFS from unit's current position
          distFromUnit = bfsDistances(grid, units, unit.y, unit.x)

          reachableTargets = []
          for ts in targetSquares
            key = posKey(ts.y, ts.x)
            if distFromUnit[key]? # Check if this target square is reachable
              reachableTargets.push { y: ts.y, x: ts.x, dist: distFromUnit[key] }

          # Proceed only if at least one target square is reachable
          if reachableTargets.length > 0
            # 3c. Choose Destination: Find the minimum distance among reachable targets
            minDist = Math.min(...reachableTargets.map (t) -> t.dist)
            # Filter for targets at the minimum distance
            nearestTargets = reachableTargets.filter (t) -> t.dist is minDist
            # Sort the nearest targets by reading order and pick the first
            nearestTargets.sort (a, b) -> a.y - b.y || a.x - b.x
            chosenDestination = nearestTargets[0]

            # 3d. Determine First Step: Find adjacent square that lies on a shortest path to chosenDestination
            # We need the shortest path distance FROM the chosen destination.
            distFromDest = bfsDistances(grid, units, chosenDestination.y, chosenDestination.x)

            bestStep = null
            # Check neighbors of the current unit in reading order
            for [dx, dy] in DIRS
              ny = unit.y + dy
              nx = unit.x + dx
              stepKey = posKey(ny, nx)

              # A valid step must be open, unoccupied, and reachable from the destination.
              # Crucially, its distance from the destination must be exactly minDist - 1
              # to ensure it's on *a* shortest path from the unit's original position.
              if ny >= 0 and ny < grid.length and nx >= 0 and nx < grid[0].length and \
                 grid[ny][nx] is '.' and \
                 not units.find((u) -> u.alive and u.y is ny and u.x is nx) and \
                 distFromDest[stepKey]? and distFromDest[stepKey] is minDist - 1

                bestStep = { y: ny, x: nx }
                break # Found the best step due to reading-order iteration of DIRS

            # 3e. Execute Move: If a valid step was found
            if bestStep?
              grid[unit.y][unit.x] = '.' # Clear current position on grid
              unit.y = bestStep.y        # Update unit's coordinates
              unit.x = bestStep.x
              grid[unit.y][unit.x] = unit.type # Mark new position on grid
              moved = true
              # After moving, check again for targets in range for the attack phase
              targetsInRange = []
              for [dx, dy] in DIRS
                nny = unit.y + dy
                nnx = unit.x + dx
                for enemy in enemies # Re-check against living enemies
                  if enemy.x is nnx and enemy.y is nny
                    targetsInRange.push enemy
                    break
      # End of Movement Phase

      # 4. Attack Phase (if targets are in range)
      if targetsInRange.length > 0
        # Select target: Lowest HP first, then reading order
        targetsInRange.sort (a, b) -> a.hp - b.hp || a.readOrder(b)
        target = targetsInRange[0]

        # Deal damage
        target.hp -= unit.ap

        # Check if target died
        if target.hp <= 0
          target.alive = false
          grid[target.y][target.x] = '.' # Make the square open cavern

          # Check for Part 2 condition: An Elf died
          if part2 and target.type is 'E'
            elfDiedInCombat = true
            # If an elf dies in part 2, this simulation is invalid, return immediately
            return { outcome: -1, elfDied: true }

    # --- End of Round ---
    break unless roundFinished # Exit main combat loop if combat ended mid-round

    roundsCompleted += 1
  # End of main combat loop

  # --- Combat Finished ---
  # Calculate sum of remaining HP
  remainingHP = units.reduce ((sum, u) -> sum + (if u.alive then u.hp else 0)), 0
  # Calculate final outcome
  outcome = roundsCompleted * remainingHP

  return { outcome, elfDied: elfDiedInCombat }

# --- Main Execution Function ---
main = ->
  # Read input file
  try
    input = fs.readFileSync('input.txt', 'utf8').trim()
  catch error
    console.error "Error reading input.txt: #{error.message}"
    console.error "Please ensure 'input.txt' exists in the same directory."
    process.exit(1)

  initialGridLines = input.split('\n')

  # Parse initial grid and unit positions *once*
  initialUnitData = []
  height = initialGridLines.length
  width = initialGridLines[0].length
  gridForParsing = initialGridLines.map (line) -> line.split('') # Use a mutable copy for parsing

  for y in [0...height]
    for x in [0...width]
      char = gridForParsing[y][x]
      if char is 'E' or char is 'G'
        # Store initial unit data, grid position will be '.'
        initialUnitData.push { type: char, x: x, y: y, ap: 3, hp: 200, alive: true }
        gridForParsing[y][x] = '.' # Replace unit with '.' on the template grid

  # Convert the parsed grid back to array of strings for passing (or keep as char array)
  # Passing initialGridLines (original) is fine, simulateCombat makes its own copy.

  # --- Part 1 ---
  console.log "--- Running Part 1 ---"
  resultP1 = simulateCombat(initialGridLines, initialUnitData, 3, false)
  console.log "Part 1 Outcome:", resultP1.outcome

  # --- Part 2 ---
  console.log "\n--- Running Part 2 ---"
  elfAP = 4 # Start checking from AP = 4
  loop
    console.log "  Trying Elf AP: #{elfAP}..."
    resultP2 = simulateCombat(initialGridLines, initialUnitData, elfAP, true)

    # Check if Elves won without any deaths
    if not resultP2.elfDied
      # Check if Elves actually won (any Goblins left?) - simulateCombat doesn't explicitly return winner type
      # We infer victory if no elf died and combat ended.
      finalUnits = initialUnitData.map (u) -> new Unit(u.type, u.x, u.y, u.ap, u.hp, u.alive)
      # Rerun *just* to get final state - slight inefficiency but clear
      finalState = simulateCombat(initialGridLines, initialUnitData, elfAP, true)
      # Re-create units from the simulation result state? No, use the return value directly.
      
      # If elfDied is false, it implies Elves won (or combat ended with only Elves)
      console.log "Part 2 Success! AP = #{elfAP}"
      console.log "Part 2 Outcome:", finalState.outcome # Use the outcome from the successful run
      break # Found the minimum AP

    elfAP += 1
    if elfAP > 200 # Safety break for potentially infinite loops
      console.error "Part 2 Error: Elf Attack Power exceeded 200. Check logic or input."
      break

# --- Program Entry Point ---
main()
