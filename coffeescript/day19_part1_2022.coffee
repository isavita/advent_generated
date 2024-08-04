fs = require 'fs'

class Blueprint
  constructor: (@id, @oreForOreRobot, @oreForClayRobot, @oreForObsidianRobot, @clayForObsidianRobot, @oreForGeodeRobot, @obsidianForGeodeRobot) ->

class State
  constructor: (@blueprint) ->
    @oreRobots = 1
    @ore = @clay = @obsidian = @geode = 0
    @clayRobots = @obsidianRobots = @geodeRobots = 0

  farm: ->
    @ore += @oreRobots
    @clay += @clayRobots
    @obsidian += @obsidianRobots
    @geode += @geodeRobots

  hash: (time) ->
    [time, @ore, @clay, @obsidian, @geode, @oreRobots, @clayRobots, @obsidianRobots, @geodeRobots].join ','

  copy: ->
    cp = Object.assign(Object.create(Object.getPrototypeOf(this)), this)
    cp.blueprint = @blueprint
    cp

  calcMostGeodes: (time, memo, totalTime, earliestGeode) ->
    return @geode if time == totalTime
    h = @hash(time)
    return memo[h] if h of memo
    return 0 if @geode == 0 && time > earliestGeode

    mostGeodes = @geode

    if @ore >= @blueprint.oreForGeodeRobot && @obsidian >= @blueprint.obsidianForGeodeRobot
      cp = @copy()
      cp.farm()
      cp.ore -= cp.blueprint.oreForGeodeRobot
      cp.obsidian -= cp.blueprint.obsidianForGeodeRobot
      cp.geodeRobots++
      earliestGeode = Math.min(earliestGeode, time + 1) if cp.geodeRobots == 1
      mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
      memo[h] = mostGeodes
      return mostGeodes

    if time <= totalTime - 16 && @oreRobots < @blueprint.oreForObsidianRobot * 2 && @ore >= @blueprint.oreForOreRobot
      cp = @copy()
      cp.ore -= cp.blueprint.oreForOreRobot
      cp.farm()
      cp.oreRobots++
      mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))

    if time <= totalTime - 8 && @clayRobots < @blueprint.clayForObsidianRobot && @ore >= @blueprint.oreForClayRobot
      cp = @copy()
      cp.ore -= cp.blueprint.oreForClayRobot
      cp.farm()
      cp.clayRobots++
      mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))

    if time <= totalTime - 4 && @obsidianRobots < @blueprint.obsidianForGeodeRobot && @ore >= @blueprint.oreForObsidianRobot && @clay >= @blueprint.clayForObsidianRobot
      cp = @copy()
      cp.ore -= cp.blueprint.oreForObsidianRobot
      cp.clay -= cp.blueprint.clayForObsidianRobot
      cp.farm()
      cp.obsidianRobots++
      mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))

    cp = @copy()
    cp.farm()
    mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))

    memo[h] = mostGeodes
    mostGeodes

parseInput = (input) ->
  lines = input.trim().split('\n')
  lines.map (line) ->
    [id, oreForOreRobot, oreForClayRobot, oreForObsidianRobot, clayForObsidianRobot, oreForGeodeRobot, obsidianForGeodeRobot] =
      line.match(/Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian./).slice(1, 8).map(Number)
    new Blueprint(id, oreForOreRobot, oreForClayRobot, oreForObsidianRobot, clayForObsidianRobot, oreForGeodeRobot, obsidianForGeodeRobot)

part1 = (input) ->
  blueprints = parseInput(input)
  sum = 0
  for bp in blueprints
    st = new State(bp)
    geodesMade = st.calcMostGeodes(0, {}, 24, 24)
    sum += st.blueprint.id * geodesMade
  sum

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  console.log part1(data)