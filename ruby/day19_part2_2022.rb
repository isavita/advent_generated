Blueprint = Struct.new(:id, :ore_cost, :clay, :obsidian, :geode)
State = Struct.new(:ore, :clay, :obsidian, :geode, :ore_robots, :clay_robots, :obsidian_robots, :geode_robots, :time_left)

def parse_blueprints(filename)
  blueprints = []
  File.foreach(filename) do |line|
    nums = line.scan(/\d+/).map(&:to_i)
    b = Blueprint.new(
      nums[0], 
      nums[1], 
      { ore_cost: nums[2] },
      { ore_cost: nums[3], clay_cost: nums[4] },
      { ore_cost: nums[5], obsidian_cost: nums[6] }
    )
    blueprints << b
  end
  blueprints
end

def max_geode(blueprint, initial_state)
  max = 0
  queue = [initial_state]
  visited = Set.new

  until queue.empty?
    state = queue.shift
    max = [max, state.geode].max

    next if state.time_left == 0

    o = [blueprint.ore_cost, blueprint.clay[:ore_cost], blueprint.obsidian[:ore_cost], blueprint.geode[:ore_cost]].max
    state.ore_robots = [state.ore_robots, o].min
    state.clay_robots = [state.clay_robots, blueprint.obsidian[:clay_cost]].min
    state.obsidian_robots = [state.obsidian_robots, blueprint.geode[:obsidian_cost]].min

    max_ore = state.time_left * o - state.ore_robots * (state.time_left - 1)
    state.ore = [state.ore, max_ore].min

    max_clay = state.time_left * blueprint.obsidian[:clay_cost] - state.clay_robots * (state.time_left - 1)
    state.clay = [state.clay, max_clay].min

    max_obsidian = state.time_left * blueprint.geode[:obsidian_cost] - state.obsidian_robots * (state.time_left - 1)
    state.obsidian = [state.obsidian, max_obsidian].min

    next if visited.include?(state)
    visited.add(state)

    queue << State.new(
      state.ore + state.ore_robots,
      state.clay + state.clay_robots,
      state.obsidian + state.obsidian_robots,
      state.geode + state.geode_robots,
      state.ore_robots,
      state.clay_robots,
      state.obsidian_robots,
      state.geode_robots,
      state.time_left - 1
    )

    if state.ore >= blueprint.ore_cost
      queue << State.new(
        state.ore - blueprint.ore_cost + state.ore_robots,
        state.clay + state.clay_robots,
        state.obsidian + state.obsidian_robots,
        state.geode + state.geode_robots,
        state.ore_robots + 1,
        state.clay_robots,
        state.obsidian_robots,
        state.geode_robots,
        state.time_left - 1
      )
    end

    if state.ore >= blueprint.clay[:ore_cost]
      queue << State.new(
        state.ore - blueprint.clay[:ore_cost] + state.ore_robots,
        state.clay + state.clay_robots,
        state.obsidian + state.obsidian_robots,
        state.geode + state.geode_robots,
        state.ore_robots,
        state.clay_robots + 1,
        state.obsidian_robots,
        state.geode_robots,
        state.time_left - 1
      )
    end

    if state.ore >= blueprint.obsidian[:ore_cost] && state.clay >= blueprint.obsidian[:clay_cost]
      queue << State.new(
        state.ore - blueprint.obsidian[:ore_cost] + state.ore_robots,
        state.clay - blueprint.obsidian[:clay_cost] + state.clay_robots,
        state.obsidian + state.obsidian_robots,
        state.geode + state.geode_robots,
        state.ore_robots,
        state.clay_robots,
        state.obsidian_robots + 1,
        state.geode_robots,
        state.time_left - 1
      )
    end

    if state.ore >= blueprint.geode[:ore_cost] && state.obsidian >= blueprint.geode[:obsidian_cost]
      queue << State.new(
        state.ore - blueprint.geode[:ore_cost] + state.ore_robots,
        state.clay + state.clay_robots,
        state.obsidian - blueprint.geode[:obsidian_cost] + state.obsidian_robots,
        state.geode + state.geode_robots,
        state.ore_robots,
        state.clay_robots,
        state.obsidian_robots,
        state.geode_robots + 1,
        state.time_left - 1
      )
    end
  end

  max
end

# Main program
blueprints = parse_blueprints('input.txt')

initial_state = State.new(0, 0, 0, 0, 1, 0, 0, 0, 32)
prod = 1

blueprints.first(3).each_with_index do |blueprint, index|
  geodes = max_geode(blueprint, initial_state)
  puts "Blueprint #{index + 1} can open #{geodes} geodes."
  prod *= geodes
end

puts "The product of the maximum geodes for the first three blueprints is: #{prod}"
