
fs = require 'fs'

parseBlueprint = (line) ->
  parts = line.split(/[:.]/)
  id = parseInt(parts[0].split(' ')[1])
  oreRobotCost = parseInt(parts[1].split(' ')[5])
  clayRobotCost = parseInt(parts[2].split(' ')[5])
  obsidianRobotCostOre = parseInt(parts[3].split(' ')[5])
  obsidianRobotCostClay = parseInt(parts[3].split(' ')[8])
  geodeRobotCostOre = parseInt(parts[4].split(' ')[5])
  geodeRobotCostObsidian = parseInt(parts[4].split(' ')[8])
  {
    id: id
    oreRobotCost: oreRobotCost
    clayRobotCost: clayRobotCost
    obsidianRobotCostOre: obsidianRobotCostOre
    obsidianRobotCostClay: obsidianRobotCostClay
    geodeRobotCostOre: geodeRobotCostOre
    geodeRobotCostObsidian: geodeRobotCostObsidian
  }

simulate = (blueprint, minutes) ->
  maxGeodes = 0
  
  dfs = (time, ore, clay, obsidian, geodes, oreRobots, clayRobots, obsidianRobots, geodeRobots) ->
    if time is minutes
      maxGeodes = Math.max(maxGeodes, geodes)
      return
    
    maxOreCost = Math.max(blueprint.oreRobotCost, blueprint.clayRobotCost, blueprint.obsidianRobotCostOre, blueprint.geodeRobotCostOre)
    
    # Pruning: If we have enough robots to produce the max cost of each resource, no need to build more
    if oreRobots >= maxOreCost
      oreRobots = maxOreCost
    if clayRobots >= blueprint.obsidianRobotCostClay
      clayRobots = blueprint.obsidianRobotCostClay
    if obsidianRobots >= blueprint.geodeRobotCostObsidian
      obsidianRobots = blueprint.geodeRobotCostObsidian
    
    # Pruning: If we can't possibly beat the current max geodes, stop
    remainingTime = minutes - time
    maxPossibleGeodes = geodes + (geodeRobots * remainingTime) + (remainingTime * (remainingTime - 1) / 2)
    if maxPossibleGeodes <= maxGeodes
      return
    
    # Always try to build a geode robot if possible
    if ore >= blueprint.geodeRobotCostOre and obsidian >= blueprint.geodeRobotCostObsidian
      dfs(time + 1, ore + oreRobots - blueprint.geodeRobotCostOre, clay + clayRobots, obsidian + obsidianRobots - blueprint.geodeRobotCostObsidian, geodes + geodeRobots, oreRobots, clayRobots, obsidianRobots, geodeRobots + 1)
      return
    
    # Try all other options
    dfs(time + 1, ore + oreRobots, clay + clayRobots, obsidian + obsidianRobots, geodes + geodeRobots, oreRobots, clayRobots, obsidianRobots, geodeRobots) # Do nothing
    if ore >= blueprint.oreRobotCost and oreRobots < maxOreCost
      dfs(time + 1, ore + oreRobots - blueprint.oreRobotCost, clay + clayRobots, obsidian + obsidianRobots, geodes + geodeRobots, oreRobots + 1, clayRobots, obsidianRobots, geodeRobots)
    if ore >= blueprint.clayRobotCost and clayRobots < blueprint.obsidianRobotCostClay
      dfs(time + 1, ore + oreRobots - blueprint.clayRobotCost, clay + clayRobots, obsidian + obsidianRobots, geodes + geodeRobots, oreRobots, clayRobots + 1, obsidianRobots, geodeRobots)
    if ore >= blueprint.obsidianRobotCostOre and clay >= blueprint.obsidianRobotCostClay and obsidianRobots < blueprint.geodeRobotCostObsidian
      dfs(time + 1, ore + oreRobots - blueprint.obsidianRobotCostOre, clay + clayRobots - blueprint.obsidianRobotCostClay, obsidian + obsidianRobots, geodes + geodeRobots, oreRobots, clayRobots, obsidianRobots + 1, geodeRobots)
  
  dfs(0, 0, 0, 0, 0, 1, 0, 0, 0)
  maxGeodes

lines = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
blueprints = lines.map(parseBlueprint)

# Part 1
qualityLevelSum = 0
for blueprint in blueprints
  maxGeodes = simulate(blueprint, 24)
  qualityLevelSum += blueprint.id * maxGeodes
console.log "Part 1:", qualityLevelSum

# Part 2
product = 1
for i in [0...3]
  maxGeodes = simulate(blueprints[i], 32)
  product *= maxGeodes
console.log "Part 2:", product
