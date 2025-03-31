
import std/[strscans, sequtils, tables, math, os, strutils, hashes]

# --- Type Definitions ---

type
  Resource* = enum Ore, Clay, Obsidian, Geode
  # Cost array: index corresponds to Resource enum (Ore, Clay, Obsidian needed)
  Cost* = array[Resource.Ore.ord .. Resource.Obsidian.ord, int]
  Robots* = array[Resource, int]       # Count of robots for each resource type
  Materials* = array[Resource, int]    # Count of materials for each resource type

  Blueprint* = object
    id*: int
    costs*: array[Resource, Cost] # costs[RobotType] gives the cost array for that robot
    # Max useful robots needed for Ore, Clay, Obsidian (index matches Resource enum)
    maxNeeded*: array[Resource.Ore.ord .. Resource.Obsidian.ord, int]

# --- Parsing ---

proc parseBlueprint(line: string): Blueprint =
  ## Parses a blueprint description string from the input file.
  # Example format: "Blueprint 1: Each ore robot costs 4 ore. ..."
  var oreOreCost, clayOreCost, obsOreCost, obsClayCost, geoOreCost, geoObsCost: int
  if not scanf(line, "Blueprint $i: Each ore robot costs $i ore. Each clay robot costs $i ore. Each obsidian robot costs $i ore and $i clay. Each geode robot costs $i ore and $i obsidian.",
               result.id, oreOreCost, clayOreCost, obsOreCost, obsClayCost, geoOreCost, geoObsCost):
    raise newException(ValueError, "Failed to parse blueprint line: " & line)

  # Store costs: costs[RobotTypeToBuild] = [OreCost, ClayCost, ObsidianCost]
  result.costs[Resource.Ore] = [oreOreCost, 0, 0]
  result.costs[Resource.Clay] = [clayOreCost, 0, 0]
  result.costs[Resource.Obsidian] = [obsOreCost, obsClayCost, 0]
  result.costs[Resource.Geode] = [geoOreCost, 0, geoObsCost]

  # Calculate max needed robots for pruning purposes.
  # We don't need more ore robots than the max ore cost of any single robot.
  result.maxNeeded[Resource.Ore.ord] = 0
  for robotType in Resource:
      result.maxNeeded[Resource.Ore.ord] = max(result.maxNeeded[Resource.Ore.ord], result.costs[robotType][Resource.Ore.ord])
  # Only obsidian robots cost clay.
  result.maxNeeded[Resource.Clay.ord] = result.costs[Resource.Obsidian][Resource.Clay.ord]
  # Only geode robots cost obsidian.
  result.maxNeeded[Resource.Obsidian.ord] = result.costs[Resource.Geode][Resource.Obsidian.ord]

# --- Simulation Core ---

proc ceilDiv(a, b: int): int =
  ## Integer division rounding up. Handles b=0 case.
  if b == 0:
    if a > 0: return int.high # Impossible to fulfill positive requirement with zero production
    else: return 0            # Needs 0 time if requirement is 0 or less
  if a <= 0: return 0          # Already have enough or more
  (a + b - 1) div b

type
  # State tuple used in recursion
  State = tuple[time: int, r: Robots, m: Materials]
  # Key for memoization table. Includes capped materials to reduce state space.
  # Capping avoids tracking excessive resources that can't be spent.
  MemoKey = tuple[time: int, r: Robots, m_capped: array[Resource.Ore.ord..Resource.Obsidian.ord, int]]

# Make the MemoKey tuple hashable for use in the Table
proc hash(k: MemoKey): Hash =
    var h: Hash = 0
    h = h !& k.time.hash
    h = h !& k.r.hash        # Hash the robot array
    h = h !& k.m_capped.hash # Hash the capped material array
    return !$h               # Finalize the hash

var
  cache: Table[MemoKey, int] # Memoization cache { state => max_geodes }
  globalBestGeodes: int      # Tracks the best result found SO FAR for the CURRENT blueprint simulation

proc solve(state: State, bp: Blueprint): int =
  ## Recursive Depth-First Search solver with time-skipping, pruning, and memoization.
  ## Finds the maximum number of geodes possible from the given state.
  let (time, r, m) = state

  # --- Base Case: Time's Up ---
  if time <= 0:
    return m[Resource.Geode]

  # --- Pruning 1: Optimistic Bound ---
  # Calculate a theoretical upper bound on geodes achievable from this state.
  # If this bound is not better than the best we've already found, prune this branch.
  # Bound = current geodes + geodes from existing robots + geodes if we build 1 new geode bot each remaining minute.
  let optimisticBound = m[Resource.Geode] + r[Resource.Geode] * time + (time * (time - 1)) div 2
  if optimisticBound <= globalBestGeodes:
    return 0 # Prune this branch, it cannot beat the current best

  # --- Memoization Check ---
  # Create the key for the cache, capping material counts.
  # Capping helps merge states that are identical except for excess resources.
  var capped_m_key: array[Resource.Ore.ord..Resource.Obsidian.ord, int]
  for i in Resource.Ore .. Resource.Obsidian:
      # Maximum useful amount of resource `i` is roughly time * max_cost_of_i
      # Any more than this likely cannot be spent in the remaining time.
      let max_useful = bp.maxNeeded[i.ord] * time
      capped_m_key[i.ord] = min(m[i], max_useful) # Cap the material count

  let key: MemoKey = (time: time, r: r, m_capped: capped_m_key)

  # Check if we've already computed the result for this state
  if cache.contains(key):
    return cache[key]

  # --- Explore Actions (Build Robots or Wait) ---

  # Baseline: Consider the geodes collected if we build nothing further from this point.
  # This provides a lower bound for the result from this state.
  var currentMaxGeodes = m[Resource.Geode] + r[Resource.Geode] * time
  # Update the global best if this path's final geode count is better.
  globalBestGeodes = max(globalBestGeodes, currentMaxGeodes)

  # Explore building each type of robot (iterate Geode -> Ore for potential prioritization)
  for robotToBuild in Resource:
    let costs = bp.costs[robotToBuild]

    # Pruning 2: Robot Cap
    # Skip building robots for resources Ore, Clay, Obsidian if we already have enough
    # to meet the maximum simultaneous demand for that resource.
    if robotToBuild != Resource.Geode and r[robotToBuild] >= bp.maxNeeded[robotToBuild.ord]:
      continue

    # Calculate time needed to gather resources for this robot
    var timeToAfford = 0
    var possible = true
    # Check Ore, Clay, Obsidian costs
    for resource in Resource.Ore .. Resource.Obsidian:
      let cost = costs[resource.ord]
      if cost > 0: # If this resource is needed for the robot
        let robotsProducing = r[resource]
        if robotsProducing == 0: # Cannot produce the required resource
          possible = false
          break
        # Time needed = ceil((cost_needed - current_material) / production_rate)
        let timeNeededForResource = ceilDiv(cost - m[resource], robotsProducing)
        timeToAfford = max(timeToAfford, timeNeededForResource)

    if not possible: continue # Can't build this type of robot ever

    # Total time taken = time to gather resources + 1 minute to build
    let timeTaken = timeToAfford + 1

    # Check if we have enough time *left* AFTER building to potentially benefit
    # (Building on the very last minute yields no resources)
    if timeTaken < time:
      # Calculate the state after 'timeTaken' minutes pass
      var next_m = m
      var next_r = r

      # 1. Collect resources during the wait and build time
      for i in Resource:
        next_m[i] += r[i] * timeTaken

      # 2. Pay the costs for the new robot
      for resource in Resource.Ore .. Resource.Obsidian:
        next_m[resource] -= costs[resource.ord]
        # assert next_m[resource] >= 0 # Sanity check: should always be affordable if possible=true

      # 3. Add the newly built robot to the fleet (it starts collecting next minute)
      next_r[robotToBuild] += 1

      # 4. Recurse: Explore the future from this new state
      let resultFromBranch = solve((time: time - timeTaken, r: next_r, m: next_m), bp)
      # Update the maximum geodes found starting from the *current* state
      currentMaxGeodes = max(currentMaxGeodes, resultFromBranch)
      # Update the overall best geodes found across all branches explored so far
      globalBestGeodes = max(globalBestGeodes, currentMaxGeodes)

  # --- Store result in cache and return ---
  cache[key] = currentMaxGeodes # Store the best result found *from this state*
  return currentMaxGeodes

# --- Main Execution Logic ---
proc run(filename: string, timeLimit: int): int =
  ## Reads blueprints, runs simulation for each, and calculates total quality level.
  var totalQualityLevel = 0

  for line in lines(filename):
    if line.strip().len == 0: continue # Skip empty lines

    let bp = parseBlueprint(line)
    # echo "Processing Blueprint ", bp.id # Optional debug print

    # Reset cache and global maximum for each blueprint simulation
    cache = initTable[MemoKey, int]()
    globalBestGeodes = 0

    # Define the initial state at minute 0
    let initialRobots: Robots = [1, 0, 0, 0]     # Start with 1 ore robot
    let initialMaterials: Materials = [0, 0, 0, 0] # Start with 0 materials
    let initialState: State = (time: timeLimit, r: initialRobots, m: initialMaterials)

    # Run the simulation/search
    let maxGeodes = solve(initialState, bp)

    echo "Blueprint ", bp.id, ": Max Geodes = ", maxGeodes
    # Calculate and accumulate the quality level
    totalQualityLevel += bp.id * maxGeodes

  return totalQualityLevel

# --- Program Entry Point ---
when isMainModule:
  let filename = "input.txt"
  # Check if the input file exists
  if not fileExists(filename):
    echo "Error: Input file '", filename, "' not found."
    quit(1) # Exit with an error code

  # Run the simulation for Part 1 (24 minutes)
  let result = run(filename, 24)
  echo "-----------------------------"
  echo "Total Quality Level: ", result
