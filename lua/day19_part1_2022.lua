local function read_file(filename)
    local file = io.open(filename, "r")
    if not file then
        error("Failed to open file: " .. filename)
    end
    local content = file:read("*a")
    file:close()
    return content
end

local function parse_input(input)
    local blueprints = {}
    for line in input:gmatch("[^\n]+") do
        local bp = {}
        local _, _, id, ore_for_ore, ore_for_clay, ore_for_obsidian, clay_for_obsidian, ore_for_geode, obsidian_for_geode = line:find("Blueprint (%d+): Each ore robot costs (%d+) ore. Each clay robot costs (%d+) ore. Each obsidian robot costs (%d+) ore and (%d+) clay. Each geode robot costs (%d+) ore and (%d+) obsidian.")
        bp.id = tonumber(id)
        bp.ore_for_ore_robot = tonumber(ore_for_ore)
        bp.ore_for_clay_robot = tonumber(ore_for_clay)
        bp.ore_for_obsidian_robot = tonumber(ore_for_obsidian)
        bp.clay_for_obsidian_robot = tonumber(clay_for_obsidian)
        bp.ore_for_geode_robot = tonumber(ore_for_geode)
        bp.obsidian_for_geode_robot = tonumber(obsidian_for_geode)
        table.insert(blueprints, bp)
    end
    return blueprints
end

local function min_int(a, b)
    return a < b and a or b
end

local function max_int(a, b)
    return a > b and a or b
end

local function new_state(blueprint)
    return {
        blueprint = blueprint,
        ore = 0,
        clay = 0,
        obsidian = 0,
        geode = 0,
        ore_robots = 1,
        clay_robots = 0,
        obsidian_robots = 0,
        geode_robots = 0
    }
end

local function farm(state)
    state.ore = state.ore + state.ore_robots
    state.clay = state.clay + state.clay_robots
    state.obsidian = state.obsidian + state.obsidian_robots
    state.geode = state.geode + state.geode_robots
end

local function hash(state, time)
    return string.format("%d,%d,%d,%d,%d,%d,%d,%d", time, state.ore, state.clay, state.obsidian, state.geode, state.ore_robots, state.clay_robots, state.obsidian_robots, state.geode_robots)
end

local function copy_state(state)
    return {
        blueprint = state.blueprint,
        ore = state.ore,
        clay = state.clay,
        obsidian = state.obsidian,
        geode = state.geode,
        ore_robots = state.ore_robots,
        clay_robots = state.clay_robots,
        obsidian_robots = state.obsidian_robots,
        geode_robots = state.geode_robots
    }
end

local function calc_most_geodes(state, time, memo, total_time, earliest_geode)
    if time == total_time then
        return state.geode
    end

    local h = hash(state, time)
    if memo[h] then
        return memo[h]
    end

    if state.geode == 0 and time > earliest_geode then
        return 0
    end

    local most_geodes = state.geode

    if state.ore >= state.blueprint.ore_for_geode_robot and state.obsidian >= state.blueprint.obsidian_for_geode_robot then
        local cp = copy_state(state)
        farm(cp)
        cp.ore = cp.ore - cp.blueprint.ore_for_geode_robot
        cp.obsidian = cp.obsidian - cp.blueprint.obsidian_for_geode_robot
        cp.geode_robots = cp.geode_robots + 1
        if cp.geode_robots == 1 then
            earliest_geode = min_int(earliest_geode, time + 1)
        end
        most_geodes = max_int(most_geodes, calc_most_geodes(cp, time + 1, memo, total_time, earliest_geode))
        memo[h] = most_geodes
        return most_geodes
    end

    if time <= total_time - 16 and state.ore_robots < state.blueprint.ore_for_obsidian_robot * 2 and state.ore >= state.blueprint.ore_for_ore_robot then
        local cp = copy_state(state)
        cp.ore = cp.ore - cp.blueprint.ore_for_ore_robot
        farm(cp)
        cp.ore_robots = cp.ore_robots + 1
        most_geodes = max_int(most_geodes, calc_most_geodes(cp, time + 1, memo, total_time, earliest_geode))
    end

    if time <= total_time - 8 and state.clay_robots < state.blueprint.clay_for_obsidian_robot and state.ore >= state.blueprint.ore_for_clay_robot then
        local cp = copy_state(state)
        cp.ore = cp.ore - cp.blueprint.ore_for_clay_robot
        farm(cp)
        cp.clay_robots = cp.clay_robots + 1
        most_geodes = max_int(most_geodes, calc_most_geodes(cp, time + 1, memo, total_time, earliest_geode))
    end

    if time <= total_time - 4 and state.obsidian_robots < state.blueprint.obsidian_for_geode_robot and state.ore >= state.blueprint.ore_for_obsidian_robot and state.clay >= state.blueprint.clay_for_obsidian_robot then
        local cp = copy_state(state)
        cp.ore = cp.ore - cp.blueprint.ore_for_obsidian_robot
        cp.clay = cp.clay - cp.blueprint.clay_for_obsidian_robot
        farm(cp)
        cp.obsidian_robots = cp.obsidian_robots + 1
        most_geodes = max_int(most_geodes, calc_most_geodes(cp, time + 1, memo, total_time, earliest_geode))
    end

    local cp = copy_state(state)
    farm(cp)
    most_geodes = max_int(most_geodes, calc_most_geodes(cp, time + 1, memo, total_time, earliest_geode))

    memo[h] = most_geodes
    return most_geodes
end

local function part1(input)
    local blueprints = parse_input(input)
    local sum = 0
    for _, bp in ipairs(blueprints) do
        local state = new_state(bp)
        local geodes_made = calc_most_geodes(state, 0, {}, 24, 24)
        sum = sum + bp.id * geodes_made
    end
    return sum
end

local input = read_file("input.txt")
print(part1(input))