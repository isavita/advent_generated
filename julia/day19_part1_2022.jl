
function solve()
    input = read("input.txt", String)
    blueprints = parse_input(input)
    sum = 0
    for bp in blueprints
        st = State(bp)
        geodes_made = calc_most_geodes(st, 0, Dict{String,Int}(), 24, 24)
        sum += bp.id * geodes_made
    end
    println(sum)
end

struct Blueprint
    id::Int
    ore_for_ore_robot::Int
    ore_for_clay_robot::Int
    ore_for_obsidian_robot::Int
    clay_for_obsidian_robot::Int
    ore_for_geode_robot::Int
    obsidian_for_geode_robot::Int
end

mutable struct State
    blueprint::Blueprint
    ore::Int
    clay::Int
    obsidian::Int
    geode::Int
    ore_robots::Int
    clay_robots::Int
    obsidian_robots::Int
    geode_robots::Int
end

State(bp::Blueprint) = State(bp, 0, 0, 0, 0, 1, 0, 0, 0)

function farm!(s::State)
    s.ore += s.ore_robots
    s.clay += s.clay_robots
    s.obsidian += s.obsidian_robots
    s.geode += s.geode_robots
end

function hash_state(s::State, time::Int)
    return string(time, s.ore, s.clay, s.obsidian, s.geode, s.ore_robots, s.clay_robots, s.obsidian_robots, s.geode_robots)
end

function copy_state(s::State)
    return State(s.blueprint, s.ore, s.clay, s.obsidian, s.geode, s.ore_robots, s.clay_robots, s.obsidian_robots, s.geode_robots)
end

function calc_most_geodes(s::State, time::Int, memo::Dict{String,Int}, total_time::Int, earliest_geode::Int)::Int
    if time == total_time
        return s.geode
    end

    h = hash_state(s, time)
    if haskey(memo, h)
        return memo[h]
    end

    if s.geode == 0 && time > earliest_geode
        return 0
    end

    most_geodes = s.geode

    if s.ore >= s.blueprint.ore_for_geode_robot && s.obsidian >= s.blueprint.obsidian_for_geode_robot
        cp = copy_state(s)
        farm!(cp)
        cp.ore -= cp.blueprint.ore_for_geode_robot
        cp.obsidian -= cp.blueprint.obsidian_for_geode_robot
        cp.geode_robots += 1
        if cp.geode_robots == 1
            earliest_geode = min(earliest_geode, time + 1)
        end
        most_geodes = max(most_geodes, calc_most_geodes(cp, time + 1, memo, total_time, earliest_geode))
        memo[h] = most_geodes
        return most_geodes
    end

    if time <= total_time - 16 && s.ore_robots < s.blueprint.ore_for_obsidian_robot * 2 && s.ore >= s.blueprint.ore_for_ore_robot
        cp = copy_state(s)
        cp.ore -= cp.blueprint.ore_for_ore_robot
        farm!(cp)
        cp.ore_robots += 1
        most_geodes = max(most_geodes, calc_most_geodes(cp, time + 1, memo, total_time, earliest_geode))
    end
    if time <= total_time - 8 && s.clay_robots < s.blueprint.clay_for_obsidian_robot && s.ore >= s.blueprint.ore_for_clay_robot
        cp = copy_state(s)
        cp.ore -= cp.blueprint.ore_for_clay_robot
        farm!(cp)
        cp.clay_robots += 1
        most_geodes = max(most_geodes, calc_most_geodes(cp, time + 1, memo, total_time, earliest_geode))
    end
    if time <= total_time - 4 && s.obsidian_robots < s.blueprint.obsidian_for_geode_robot && s.ore >= s.blueprint.ore_for_obsidian_robot && s.clay >= s.blueprint.clay_for_obsidian_robot
        cp = copy_state(s)
        cp.ore -= cp.blueprint.ore_for_obsidian_robot
        cp.clay -= cp.blueprint.clay_for_obsidian_robot
        farm!(cp)
        cp.obsidian_robots += 1
        most_geodes = max(most_geodes, calc_most_geodes(cp, time + 1, memo, total_time, earliest_geode))
    end

    cp = copy_state(s)
    farm!(cp)
    most_geodes = max(most_geodes, calc_most_geodes(cp, time + 1, memo, total_time, earliest_geode))

    memo[h] = most_geodes
    return most_geodes
end

function parse_input(input::String)::Vector{Blueprint}
    blueprints = Blueprint[]
    for line in split(strip(input), "\n")
        parts = map(x -> parse(Int, x), match(r"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.", line).captures)
        bp = Blueprint(parts[1], parts[2], parts[3], parts[4], parts[5], parts[6], parts[7])
        push!(blueprints, bp)
    end
    return blueprints
end

solve()
