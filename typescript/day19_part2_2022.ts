import * as fs from "fs"
class Blueprint {
  constructor(public id: number, public ore_cost: number, public clay_ore_cost: number, public obsidian_ore_cost: number, public obsidian_clay_cost: number, public geode_ore_cost: number, public geode_obsidian_cost: number) {}
}
class State {
  constructor(public ore = 0, public clay = 0, public obsidian = 0, public geode = 0, public ore_robots = 0, public clay_robots = 0, public obsidian_robots = 0, public geode_robots = 0, public time_left = 0) {}
}
function max_geode(b: Blueprint, st: State): number {
  let max_geodes = 0
  let queue: State[] = [st]
  let visited = new Set<string>()
  let idx = 0
  while (idx < queue.length) {
    let s = queue[idx++]
    max_geodes = Math.max(max_geodes, s.geode)
    if (s.time_left === 0) continue
    let o = Math.max(b.ore_cost, b.clay_ore_cost, b.obsidian_ore_cost, b.geode_ore_cost)
    if (s.ore_robots >= o) s.ore_robots = o
    if (s.clay_robots >= b.obsidian_clay_cost) s.clay_robots = b.obsidian_clay_cost
    if (s.obsidian_robots >= b.geode_obsidian_cost) s.obsidian_robots = b.geode_obsidian_cost
    let max_ore = s.time_left * o - s.ore_robots * (s.time_left - 1)
    if (s.ore >= max_ore) s.ore = max_ore
    let max_clay = s.time_left * b.obsidian_clay_cost - s.clay_robots * (s.time_left - 1)
    if (s.clay >= max_clay) s.clay = max_clay
    let max_obsidian = s.time_left * b.geode_obsidian_cost - s.obsidian_robots * (s.time_left - 1)
    if (s.obsidian >= max_obsidian) s.obsidian = max_obsidian
    let key = `${s.ore},${s.clay},${s.obsidian},${s.geode},${s.ore_robots},${s.clay_robots},${s.obsidian_robots},${s.geode_robots},${s.time_left}`
    if (visited.has(key)) continue
    visited.add(key)
    queue.push(new State(s.ore+s.ore_robots, s.clay+s.clay_robots, s.obsidian+s.obsidian_robots, s.geode+s.geode_robots, s.ore_robots, s.clay_robots, s.obsidian_robots, s.geode_robots, s.time_left-1))
    if (s.ore >= b.ore_cost)
      queue.push(new State(s.ore-b.ore_cost+s.ore_robots, s.clay+s.clay_robots, s.obsidian+s.obsidian_robots, s.geode+s.geode_robots, s.ore_robots+1, s.clay_robots, s.obsidian_robots, s.geode_robots, s.time_left-1))
    if (s.ore >= b.clay_ore_cost)
      queue.push(new State(s.ore-b.clay_ore_cost+s.ore_robots, s.clay+s.clay_robots, s.obsidian+s.obsidian_robots, s.geode+s.geode_robots, s.ore_robots, s.clay_robots+1, s.obsidian_robots, s.geode_robots, s.time_left-1))
    if (s.ore >= b.obsidian_ore_cost && s.clay >= b.obsidian_clay_cost)
      queue.push(new State(s.ore-b.obsidian_ore_cost+s.ore_robots, s.clay-b.obsidian_clay_cost+s.clay_robots, s.obsidian+s.obsidian_robots, s.geode+s.geode_robots, s.ore_robots, s.clay_robots, s.obsidian_robots+1, s.geode_robots, s.time_left-1))
    if (s.ore >= b.geode_ore_cost && s.obsidian >= b.geode_obsidian_cost)
      queue.push(new State(s.ore-b.geode_ore_cost+s.ore_robots, s.clay+s.clay_robots, s.obsidian-b.geode_obsidian_cost+s.obsidian_robots, s.geode+s.geode_robots, s.ore_robots, s.clay_robots, s.obsidian_robots, s.geode_robots+1, s.time_left-1))
  }
  return max_geodes
}
const blueprints: Blueprint[] = []
const input = fs.readFileSync("input.txt", "utf8").trim().split("\n")
for(const line of input) {
  const parts = line.trim().split(/\s+/)
  const id = parseInt(parts[1].slice(0,-1))
  const ore_cost = parseInt(parts[6])
  const clay_ore_cost = parseInt(parts[12])
  const obsidian_ore_cost = parseInt(parts[18])
  const obsidian_clay_cost = parseInt(parts[21])
  const geode_ore_cost = parseInt(parts[27])
  const geode_obsidian_cost = parseInt(parts[30])
  blueprints.push(new Blueprint(id, ore_cost, clay_ore_cost, obsidian_ore_cost, obsidian_clay_cost, geode_ore_cost, geode_obsidian_cost))
}
const init = new State(0,0,0,0,1,0,0,0,32)
let prod = 1
for(const b of blueprints.slice(0,3))
  prod *= max_geode(b, init)
console.log(prod)

