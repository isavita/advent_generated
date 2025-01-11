
record Blueprint, ore_cost : Int32, clay_cost : Int32, obsidian_cost : Tuple(Int32, Int32), geode_cost : Tuple(Int32, Int32)

def max_geodes(blueprint : Blueprint, time_limit : Int32)
  max_ore_needed = [blueprint.ore_cost, blueprint.clay_cost, blueprint.obsidian_cost[0], blueprint.geode_cost[0]].max
  max_clay_needed = blueprint.obsidian_cost[1]
  max_obsidian_needed = blueprint.geode_cost[1]

  q = [] of Tuple(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)
  q.push({time_limit, 0, 0, 0, 0, 1, 0, 0})
  visited = Set(Tuple(Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32)).new
  max_g = 0

  until q.empty?
    time, ore, clay, obsidian, geodes, ore_r, clay_r, obsidian_r = q.shift

    max_g = [max_g, geodes].max

    next if time == 0

    new_ore_r = ore_r
    new_clay_r = clay_r
    new_obsidian_r = obsidian_r
    new_geodes = geodes

    if ore >= blueprint.geode_cost[0] && obsidian >= blueprint.geode_cost[1]
      new_ore = ore - blueprint.geode_cost[0] + ore_r
      new_clay = clay + clay_r
      new_obsidian = obsidian - blueprint.geode_cost[1] + obsidian_r
      new_geodes += time - 1
      state = {time - 1, new_ore, new_clay, new_obsidian, new_geodes, ore_r, clay_r, obsidian_r}
      unless visited.includes?(state)
        visited.add(state)
        q.push(state)
      end
      next
    end

    if ore >= blueprint.obsidian_cost[0] && clay >= blueprint.obsidian_cost[1] && obsidian_r < max_obsidian_needed
      new_ore = ore - blueprint.obsidian_cost[0] + ore_r
      new_clay = clay - blueprint.obsidian_cost[1] + clay_r
      new_obsidian = obsidian + obsidian_r
      state = {time - 1, new_ore, new_clay, new_obsidian, new_geodes, ore_r, clay_r, obsidian_r + 1}
      unless visited.includes?(state)
        visited.add(state)
        q.push(state)
      end
    end

    if ore >= blueprint.clay_cost && clay_r < max_clay_needed
      new_ore = ore - blueprint.clay_cost + ore_r
      new_clay = clay + clay_r
      new_obsidian = obsidian + obsidian_r
      state = {time - 1, new_ore, new_clay, new_obsidian, new_geodes, ore_r, clay_r + 1, obsidian_r}
      unless visited.includes?(state)
        visited.add(state)
        q.push(state)
      end
    end

    if ore >= blueprint.ore_cost && ore_r < max_ore_needed
      new_ore = ore - blueprint.ore_cost + ore_r
      new_clay = clay + clay_r
      new_obsidian = obsidian + obsidian_r
      state = {time - 1, new_ore, new_clay, new_obsidian, new_geodes, ore_r + 1, clay_r, obsidian_r}
      unless visited.includes?(state)
        visited.add(state)
        q.push(state)
      end
    end

    new_ore = ore + ore_r
    new_clay = clay + clay_r
    new_obsidian = obsidian + obsidian_r
    state = {time - 1, new_ore, new_clay, new_obsidian, new_geodes, ore_r, clay_r, obsidian_r}
    unless visited.includes?(state)
      visited.add(state)
      q.push(state)
    end
  end
  max_g
end

blueprints = [] of Blueprint
File.each_line("input.txt") do |line|
  m = line.match(/Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian./)
  if m
    blueprints << Blueprint.new(m[2].to_i32, m[3].to_i32, {m[4].to_i32, m[5].to_i32}, {m[6].to_i32, m[7].to_i32})
  end
end

total_quality_level = 0
blueprints.each_with_index do |blueprint, index|
  geodes = max_geodes(blueprint, 24)
  total_quality_level += (index + 1) * geodes
end

puts total_quality_level
