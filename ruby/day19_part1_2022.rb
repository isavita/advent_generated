Blueprint = Struct.new(:id, :ore_robot, :clay_robot, :obsidian_robot, :geode_robot)

def parse_blueprints(input)
  input.split("\n").map do |line|
    nums = line.scan(/\d+/).map(&:to_i)
    Blueprint.new(nums[0], nums[1], nums[2], [nums[3], nums[4]], [nums[5], nums[6]])
  end
end

def max_geodes(blueprint, time_limit)
  max_ore_cost = [blueprint.ore_robot, blueprint.clay_robot, blueprint.obsidian_robot[0], blueprint.geode_robot[0]].max
  
  initial_state = [0, 0, 0, 0, 1, 0, 0, 0, time_limit].freeze
  states = [initial_state]
  best = 0
  cache = {}

  until states.empty?
    ore, clay, obsidian, geodes, ore_bots, clay_bots, obsidian_bots, geode_bots, time = states.pop
    
    best = [best, geodes].max

    break if time == 0

    key = [ore, clay, obsidian, geodes, ore_bots, clay_bots, obsidian_bots, geode_bots, time]
    next if cache[key] && cache[key] >= geodes
    cache[key] = geodes

    potential = geodes + (time * (time - 1) / 2)
    next if potential <= best

    ore_income, clay_income, obsidian_income = ore_bots, clay_bots, obsidian_bots

    # Try building geode robot
    if obsidian >= blueprint.geode_robot[1] && ore >= blueprint.geode_robot[0]
      new_state = [
        ore + ore_income - blueprint.geode_robot[0],
        clay + clay_income,
        obsidian + obsidian_income - blueprint.geode_robot[1],
        geodes + time - 1,
        ore_bots, clay_bots, obsidian_bots, geode_bots + 1,
        time - 1
      ].freeze
      states << new_state
      next
    end

    # Try building obsidian robot
    if clay >= blueprint.obsidian_robot[1] && ore >= blueprint.obsidian_robot[0] && obsidian_bots < blueprint.geode_robot[1]
      new_state = [
        ore + ore_income - blueprint.obsidian_robot[0],
        clay + clay_income - blueprint.obsidian_robot[1],
        obsidian + obsidian_income,
        geodes,
        ore_bots, clay_bots, obsidian_bots + 1, geode_bots,
        time - 1
      ].freeze
      states << new_state
    end

    # Try building clay robot
    if ore >= blueprint.clay_robot && clay_bots < blueprint.obsidian_robot[1]
      new_state = [
        ore + ore_income - blueprint.clay_robot,
        clay + clay_income,
        obsidian + obsidian_income,
        geodes,
        ore_bots, clay_bots + 1, obsidian_bots, geode_bots,
        time - 1
      ].freeze
      states << new_state
    end

    # Try building ore robot
    if ore >= blueprint.ore_robot && ore_bots < max_ore_cost
      new_state = [
        ore + ore_income - blueprint.ore_robot,
        clay + clay_income,
        obsidian + obsidian_income,
        geodes,
        ore_bots + 1, clay_bots, obsidian_bots, geode_bots,
        time - 1
      ].freeze
      states << new_state
    end

    # Try not building any robot
    if ore < 2 * max_ore_cost
      new_state = [
        ore + ore_income,
        clay + clay_income,
        obsidian + obsidian_income,
        geodes,
        ore_bots, clay_bots, obsidian_bots, geode_bots,
        time - 1
      ].freeze
      states << new_state
    end
  end

  best
end

def calculate_quality_levels(blueprints, time_limit)
  blueprints.map do |blueprint|
    geodes = max_geodes(blueprint, time_limit)
    blueprint.id * geodes
  end
end

# Read input from file
input = File.read('input.txt')

# Parse blueprints
blueprints = parse_blueprints(input)

# Calculate quality levels
quality_levels = calculate_quality_levels(blueprints, 24)

# Sum up quality levels
total_quality = quality_levels.sum

puts "The sum of the quality levels is: #{total_quality}"
