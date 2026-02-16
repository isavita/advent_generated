
import std/[deques, sets, strutils, os]

type
  Blueprint = object
    id, oreCost, clayOreCost, obsidianOreCost, obsidianClayCost, geodeOreCost, geodeObsidianCost: int

  State = object
    ore, clay, obsidian, geode: int
    oreRobots, clayRobots, obsidianRobots, geodeRobots, timeLeft: int

proc maxGeode(b: Blueprint, st: State): int =
  var q = initDeque[State]()
  q.addLast(st)
  var visited: HashSet[string]
  while q.len > 0:
    let s = q.popFirst()
    result = max(result, s.geode)
    if s.timeLeft == 0: continue
    let maxOreCost = max(max(b.oreCost, b.clayOreCost), max(b.obsidianOreCost, b.geodeOreCost))
    var ns = s
    if ns.oreRobots >= maxOreCost: ns.oreRobots = maxOreCost
    if ns.clayRobots >= b.obsidianClayCost: ns.clayRobots = b.obsidianClayCost
    if ns.obsidianRobots >= b.geodeObsidianCost: ns.obsidianRobots = b.geodeObsidianCost
    let maxOre = ns.timeLeft * maxOreCost - ns.oreRobots * (ns.timeLeft - 1)
    if ns.ore >= maxOre: ns.ore = maxOre
    let maxClay = ns.timeLeft * b.obsidianClayCost - ns.clayRobots * (ns.timeLeft - 1)
    if ns.clay >= maxClay: ns.clay = maxClay
    let maxObsidian = ns.timeLeft * b.geodeObsidianCost - ns.obsidianRobots * (ns.timeLeft - 1)
    if ns.obsidian >= maxObsidian: ns.obsidian = maxObsidian
    let key = $ns.ore & "," & $ns.clay & "," & $ns.obsidian & "," & $ns.geode & "," &
              $ns.oreRobots & "," & $ns.clayRobots & "," & $ns.obsidianRobots & "," & $ns.geodeRobots & "," & $ns.timeLeft
    if visited.contains(key): continue
    visited.incl(key)
    q.addLast State(ore: ns.ore + ns.oreRobots, clay: ns.clay + ns.clayRobots,
                    obsidian: ns.obsidian + ns.obsidianRobots, geode: ns.geode + ns.geodeRobots,
                    oreRobots: ns.oreRobots, clayRobots: ns.clayRobots,
                    obsidianRobots: ns.obsidianRobots, geodeRobots: ns.geodeRobots,
                    timeLeft: ns.timeLeft - 1)
    if ns.ore >= b.oreCost:
      q.addLast State(ore: ns.ore - b.oreCost + ns.oreRobots, clay: ns.clay + ns.clayRobots,
                      obsidian: ns.obsidian + ns.obsidianRobots, geode: ns.geode + ns.geodeRobots,
                      oreRobots: ns.oreRobots + 1, clayRobots: ns.clayRobots,
                      obsidianRobots: ns.obsidianRobots, geodeRobots: ns.geodeRobots,
                      timeLeft: ns.timeLeft - 1)
    if ns.ore >= b.clayOreCost:
      q.addLast State(ore: ns.ore - b.clayOreCost + ns.oreRobots, clay: ns.clay + ns.clayRobots,
                      obsidian: ns.obsidian + ns.obsidianRobots, geode: ns.geode + ns.geodeRobots,
                      oreRobots: ns.oreRobots, clayRobots: ns.clayRobots + 1,
                      obsidianRobots: ns.obsidianRobots, geodeRobots: ns.geodeRobots,
                      timeLeft: ns.timeLeft - 1)
    if ns.ore >= b.obsidianOreCost and ns.clay >= b.obsidianClayCost:
      q.addLast State(ore: ns.ore - b.obsidianOreCost + ns.oreRobots,
                      clay: ns.clay - b.obsidianClayCost + ns.clayRobots,
                      obsidian: ns.obsidian + ns.obsidianRobots, geode: ns.geode + ns.geodeRobots,
                      oreRobots: ns.oreRobots, clayRobots: ns.clayRobots,
                      obsidianRobots: ns.obsidianRobots + 1, geodeRobots: ns.geodeRobots,
                      timeLeft: ns.timeLeft - 1)
    if ns.ore >= b.geodeOreCost and ns.obsidian >= b.geodeObsidianCost:
      q.addLast State(ore: ns.ore - b.geodeOreCost + ns.oreRobots, clay: ns.clay + ns.clayRobots,
                      obsidian: ns.obsidian - b.geodeObsidianCost + ns.obsidianRobots,
                      geode: ns.geode + ns.geodeRobots,
                      oreRobots: ns.oreRobots, clayRobots: ns.clayRobots,
                      obsidianRobots: ns.obsidianRobots, geodeRobots: ns.geodeRobots + 1,
                      timeLeft: ns.timeLeft - 1)

var blueprints: seq[Blueprint]
for line in lines("input.txt"):
  let p = line.split()
  blueprints.add Blueprint(
    id: parseInt(p[1][0..^2]),
    oreCost: parseInt(p[6]),
    clayOreCost: parseInt(p[12]),
    obsidianOreCost: parseInt(p[18]),
    obsidianClayCost: parseInt(p[21]),
    geodeOreCost: parseInt(p[27]),
    geodeObsidianCost: parseInt(p[30])
  )

let init = State(oreRobots: 1, timeLeft: 32)
var prod = 1
for i in 0..min(2, blueprints.high):
  prod *= maxGeode(blueprints[i], init)
echo prod
