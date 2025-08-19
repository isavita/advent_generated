
require "deque"
require "set"

struct Blueprint
  property id : Int32
  property ore_cost : Int32
  property clay_ore_cost : Int32
  property obsidian_ore_cost : Int32
  property obsidian_clay_cost : Int32
  property geode_ore_cost : Int32
  property geode_obsidian_cost : Int32
  def initialize(@id, @ore_cost, @clay_ore_cost, @obsidian_ore_cost,
                 @obsidian_clay_cost, @geode_ore_cost, @geode_obsidian_cost)
  end
end

struct State
  property ore : Int32
  property clay : Int32
  property obsidian : Int32
  property geode : Int32
  property ore_robots : Int32
  property clay_robots : Int32
  property obsidian_robots : Int32
  property geode_robots : Int32
  property time_left : Int32
  def initialize(@ore = 0, @clay = 0, @obsidian = 0, @geode = 0,
                 @ore_robots = 0, @clay_robots = 0, @obsidian_robots = 0,
                 @geode_robots = 0, @time_left = 0)
  end
end

def max_geode(b : Blueprint, init : State) : Int32
  max = 0
  q = Deque(State).new
  q << init
  visited = Set(Tuple(Int32,Int32,Int32,Int32,Int32,Int32,Int32,Int32,Int32)).new
  while (s = q.shift?)
    max = s.geode if s.geode > max
    next if s.time_left == 0
    o = {b.ore_cost, b.clay_ore_cost, b.obsidian_ore_cost, b.geode_ore_cost}.max
    s.ore_robots = o if s.ore_robots > o
    s.clay_robots = b.obsidian_clay_cost if s.clay_robots > b.obsidian_clay_cost
    s.obsidian_robots = b.geode_obsidian_cost if s.obsidian_robots > b.geode_obsidian_cost
    max_ore = s.time_left * o - s.ore_robots * (s.time_left - 1)
    s.ore = max_ore if s.ore > max_ore
    max_clay = s.time_left * b.obsidian_clay_cost - s.clay_robots * (s.time_left - 1)
    s.clay = max_clay if s.clay > max_clay
    max_obs = s.time_left * b.geode_obsidian_cost - s.obsidian_robots * (s.time_left - 1)
    s.obsidian = max_obs if s.obsidian > max_obs
    key = {s.ore, s.clay, s.obsidian, s.geode,
           s.ore_robots, s.clay_robots, s.obsidian_robots,
           s.geode_robots, s.time_left}
    next if visited.includes?(key)
    visited << key
    q << State.new(s.ore + s.ore_robots,
                   s.clay + s.clay_robots,
                   s.obsidian + s.obsidian_robots,
                   s.geode + s.geode_robots,
                   s.ore_robots, s.clay_robots,
                   s.obsidian_robots, s.geode_robots,
                   s.time_left - 1)
    if s.ore >= b.ore_cost
      q << State.new(s.ore - b.ore_cost + s.ore_robots,
                     s.clay + s.clay_robots,
                     s.obsidian + s.obsidian_robots,
                     s.geode + s.geode_robots,
                     s.ore_robots + 1, s.clay_robots,
                     s.obsidian_robots, s.geode_robots,
                     s.time_left - 1)
    end
    if s.ore >= b.clay_ore_cost
      q << State.new(s.ore - b.clay_ore_cost + s.ore_robots,
                     s.clay + s.clay_robots,
                     s.obsidian + s.obsidian_robots,
                     s.geode + s.geode_robots,
                     s.ore_robots, s.clay_robots + 1,
                     s.obsidian_robots, s.geode_robots,
                     s.time_left - 1)
    end
    if s.ore >= b.obsidian_ore_cost && s.clay >= b.obsidian_clay_cost
      q << State.new(s.ore - b.obsidian_ore_cost + s.ore_robots,
                     s.clay - b.obsidian_clay_cost + s.clay_robots,
                     s.obsidian + s.obsidian_robots,
                     s.geode + s.geode_robots,
                     s.ore_robots, s.clay_robots,
                     s.obsidian_robots + 1, s.geode_robots,
                     s.time_left - 1)
    end
    if s.ore >= b.geode_ore_cost && s.obsidian >= b.geode_obsidian_cost
      q << State.new(s.ore - b.geode_ore_cost + s.ore_robots,
                     s.clay + s.clay_robots,
                     s.obsidian - b.geode_obsidian_cost + s.obsidian_robots,
                     s.geode + s.geode_robots,
                     s.ore_robots, s.clay_robots,
                     s.obsidian_robots, s.geode_robots + 1,
                     s.time_left - 1)
    end
  end
  max
end

blueprints = [] of Blueprint
File.each_line("input.txt") do |line|
  parts = line.split
  id = parts[1].chomp(':').to_i
  ore = parts[6].to_i
  clay = parts[12].to_i
  obs_ore = parts[18].to_i
  obs_clay = parts[21].to_i
  geo_ore = parts[27].to_i
  geo_obs = parts[30].to_i
  blueprints << Blueprint.new(id, ore, clay, obs_ore, obs_clay, geo_ore, geo_obs)
end

init = State.new(ore_robots: 1, time_left: 32)
product = 1_i64
blueprints.first(3).each do |b|
  product *= max_geode(b, init)
end
puts product
