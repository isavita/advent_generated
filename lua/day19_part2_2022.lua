
--[[
    Advent of Code 2022 - Day 19: Not Enough Minerals
    Lua solution implementing DFS with memoization and pruning.
]]

local M = {}

-- Use integer division if available (Lua 5.3+), otherwise fallback
local floor_div = math.floor or function(a, b) return math.floor(a / b) end

-- Memoization table and global max for pruning
local memo = {}
local max_geodes_found = 0

-- Blueprint structure and max costs precomputed
local blueprints = {}
local Blueprint = {}
Blueprint.__index = Blueprint

function Blueprint:new(id, ore_ore, clay_ore, obs_ore, obs_clay, geode_ore, geode_obs)
    local obj = setmetatable({}, Blueprint)
    obj.id = id
    obj.ore_ore = ore_ore
    obj.clay_ore = clay_ore
    obj.obs_ore = obs_ore
    obj.obs_clay = obs_clay
    obj.geode_ore = geode_ore
    obj.geode_obs = geode_obs

    -- Precompute the maximum ore needed for any robot type
    obj.max_ore_cost = math.max(ore_ore, clay_ore, obs_ore, geode_ore)
    -- Max clay needed (only for obsidian robots)
    obj.max_clay_cost = obs_clay
    -- Max obsidian needed (only for geode robots)
    obj.max_obs_cost = geode_obs

    return obj
end

-- Parses the input file and populates the blueprints table
function M.parse_input(filename)
    local file = io.open(filename, "r")
    if not file then
        error("Could not open file: " .. filename)
    end

    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()

    blueprints = {} -- Clear previous blueprints if any

    for _, line in ipairs(lines) do
        local id, ore_ore, clay_ore, obs_ore, obs_clay, geode_ore, geode_obs =
            line:match("Blueprint (%d+): Each ore robot costs (%d+) ore%. Each clay robot costs (%d+) ore%. Each obsidian robot costs (%d+) ore and (%d+) clay%. Each geode robot costs (%d+) ore and (%d+) obsidian%.")

        if id then
            table.insert(blueprints, Blueprint:new(
                tonumber(id),
                tonumber(ore_ore),
                tonumber(clay_ore),
                tonumber(obs_ore),
                tonumber(obs_clay),
                tonumber(geode_ore),
                tonumber(geode_obs)
            ))
        else
            print("Warning: Could not parse line: " .. line)
        end
    end
end

--[[
    Recursive DFS function to find the maximum geodes.
    State representation:
    bp: blueprint costs
    time_left: minutes remaining
    r1, r2, r3, r4: number of ore, clay, obsidian, geode robots
    res1, res2, res3, res4: amount of ore, clay, obsidian, geode resources
]]
function M.find_max_geodes(bp, time_left, r1, r2, r3, r4, res1, res2, res3, res4)
    -- Base case: Time's up
    if time_left == 0 then
        max_geodes_found = math.max(max_geodes_found, res4)
        return res4
    end

    -- Pruning 1: Check optimistic upper bound
    -- Maximum possible geodes if we build a geode bot every remaining minute
    local potential_max = res4 + r4 * time_left + floor_div(time_left * (time_left - 1), 2)
    if potential_max <= max_geodes_found then
        return 0 -- Prune this branch, can't beat the current best
    end

    -- Memoization Key: Combine all state variables
    -- Note: Resource amounts can grow large. We could potentially cap them
    -- for the memo key, but it risks incorrect results if the cap is too low.
    -- Using the full state seems safer, though potentially uses more memory.
    -- String concatenation is reasonably fast in LuaJIT, acceptable in standard Lua.
    local key = table.concat({time_left, r1, r2, r3, r4, res1, res2, res3, res4}, ",")
    -- Alternatively for potentially better performance if state space allows:
    -- local key = time_left * 1e12 + r1 * 1e10 + r2 * 1e8 + r3 * 1e6 + r4 * 1e4 + res1 * 1e3 + res2 * 1e2 + res3 * 1e1 + res4
    -- Requires careful bounds checking - stick with string for robustness.

    if memo[key] then
        return memo[key]
    end

    -- Simulate resource collection for the current minute
    local next_res1 = res1 + r1
    local next_res2 = res2 + r2
    local next_res3 = res3 + r3
    local next_res4 = res4 + r4

    local current_max = 0

    -- Decision: Build which robot? (Or none)
    -- Explore options in a potentially beneficial order (Geode > Obsidian > Clay > Ore > Wait)

    -- Option 4: Build Geode Robot
    -- Can we afford it with CURRENT resources?
    if res1 >= bp.geode_ore and res3 >= bp.geode_obs then
        current_max = math.max(current_max, M.find_max_geodes(
            bp, time_left - 1,
            r1, r2, r3, r4 + 1, -- Robot added next minute
            next_res1 - bp.geode_ore, -- Resources spent (from collected amount)
            next_res2,
            next_res3 - bp.geode_obs,
            next_res4
        ))
        -- Optimization: If we can build a geode robot, maybe don't explore other builds this turn?
        -- This is a common heuristic, but can be wrong. Let's explore all options for correctness first.
        -- If performance is an issue, revisit this. For now, remove the 'else' structure.
    end

    -- Option 3: Build Obsidian Robot
    -- Pruning 2: Don't build more resource robots than needed per minute
    -- Can we afford it?
    if r3 < bp.max_obs_cost and res1 >= bp.obs_ore and res2 >= bp.obs_clay then
         current_max = math.max(current_max, M.find_max_geodes(
            bp, time_left - 1,
            r1, r2, r3 + 1, r4,
            next_res1 - bp.obs_ore,
            next_res2 - bp.obs_clay,
            next_res3,
            next_res4
        ))
    end

    -- Option 2: Build Clay Robot
    -- Pruning 2: Don't build more resource robots than needed per minute
    -- Can we afford it?
    if r2 < bp.max_clay_cost and res1 >= bp.clay_ore then
         current_max = math.max(current_max, M.find_max_geodes(
            bp, time_left - 1,
            r1, r2 + 1, r3, r4,
            next_res1 - bp.clay_ore,
            next_res2,
            next_res3,
            next_res4
        ))
    end

    -- Option 1: Build Ore Robot
    -- Pruning 2: Don't build more resource robots than needed per minute
    -- Can we afford it?
    if r1 < bp.max_ore_cost and res1 >= bp.ore_ore then
         current_max = math.max(current_max, M.find_max_geodes(
            bp, time_left - 1,
            r1 + 1, r2, r3, r4,
            next_res1 - bp.ore_ore,
            next_res2,
            next_res3,
            next_res4
        ))
    end

    -- Option 0: Build Nothing (Wait)
    -- Always consider waiting
    current_max = math.max(current_max, M.find_max_geodes(
        bp, time_left - 1,
        r1, r2, r3, r4,      -- Robots unchanged
        next_res1, next_res2, next_res3, next_res4 -- Resources accumulate
    ))

    -- Store result in memo table before returning
    memo[key] = current_max
    return current_max
end


-- Main execution function
function M.main()
    M.parse_input("input.txt")

    -- Part 1
    local total_quality_level = 0
    local time_limit_part1 = 24
    print("--- Part 1 ---")
    for i, bp in ipairs(blueprints) do
        memo = {} -- Clear memoization table for each blueprint
        max_geodes_found = 0 -- Reset global max for pruning
        local geodes = M.find_max_geodes(bp, time_limit_part1, 1, 0, 0, 0, 0, 0, 0, 0)
        local quality_level = bp.id * geodes
        total_quality_level = total_quality_level + quality_level
        print(string.format("Blueprint %d: Max Geodes = %d, Quality Level = %d", bp.id, geodes, quality_level))
    end
    print("Total Quality Level (Part 1):", total_quality_level)

    -- Part 2
    local product_of_geodes = 1
    local time_limit_part2 = 32
    print("\n--- Part 2 ---")
    for i = 1, math.min(3, #blueprints) do -- Only first three blueprints
        local bp = blueprints[i]
        memo = {} -- Clear memoization table
        max_geodes_found = 0 -- Reset global max
        local geodes = M.find_max_geodes(bp, time_limit_part2, 1, 0, 0, 0, 0, 0, 0, 0)
        product_of_geodes = product_of_geodes * geodes
        print(string.format("Blueprint %d: Max Geodes = %d", bp.id, geodes))
    end
    print("Product of Max Geodes (Part 2):", product_of_geodes)

end

-- Entry point
M.main()

-- Return the module table (optional, good practice if used as a library)
return M
