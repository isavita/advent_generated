
struct Blueprint
    id::Int
    oreCost::Int
    clayCost::Int
    obsidianCostOre::Int
    obsidianCostClay::Int
    geodeCostOre::Int
    geodeCostObsidian::Int
end

struct State
    ore::Int
    clay::Int
    obsidian::Int
    geode::Int
    oreRobots::Int
    clayRobots::Int
    obsidianRobots::Int
    geodeRobots::Int
    timeLeft::Int
end

function parseBlueprint(line::String)::Blueprint
    m = match(r"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.", line)
    return Blueprint(parse(Int, m.captures[1]), parse(Int, m.captures[2]), parse(Int, m.captures[3]), parse(Int, m.captures[4]), parse(Int, m.captures[5]), parse(Int, m.captures[6]), parse(Int, m.captures[7]))
end

function maxGeode(b::Blueprint, st::State)::Int
    maxGeodes = 0
    queue = [st]
    visited = Set{State}()
    while !isempty(queue)
        s = popfirst!(queue)
        maxGeodes = max(maxGeodes, s.geode)
        if s.timeLeft == 0
            continue
        end
        maxOre = s.timeLeft * max(b.oreCost, b.clayCost, b.obsidianCostOre, b.geodeCostOre) - s.oreRobots * (s.timeLeft - 1)
        maxClay = s.timeLeft * b.obsidianCostClay - s.clayRobots * (s.timeLeft - 1)
        maxObsidian = s.timeLeft * b.geodeCostObsidian - s.obsidianRobots * (s.timeLeft - 1)
        s = State(min(s.ore, maxOre), min(s.clay, maxClay), min(s.obsidian, maxObsidian), s.geode, s.oreRobots, s.clayRobots, s.obsidianRobots, s.geodeRobots, s.timeLeft)
        if s in visited
            continue
        end
        push!(visited, s)
        push!(queue, State(s.ore + s.oreRobots, s.clay + s.clayRobots, s.obsidian + s.obsidianRobots, s.geode + s.geodeRobots, s.oreRobots, s.clayRobots, s.obsidianRobots, s.geodeRobots, s.timeLeft - 1))
        if s.ore >= b.oreCost
            push!(queue, State(s.ore - b.oreCost + s.oreRobots, s.clay + s.clayRobots, s.obsidian + s.obsidianRobots, s.geode + s.geodeRobots, s.oreRobots + 1, s.clayRobots, s.obsidianRobots, s.geodeRobots, s.timeLeft - 1))
        end
        if s.ore >= b.clayCost
            push!(queue, State(s.ore - b.clayCost + s.oreRobots, s.clay + s.clayRobots, s.obsidian + s.obsidianRobots, s.geode + s.geodeRobots, s.oreRobots, s.clayRobots + 1, s.obsidianRobots, s.geodeRobots, s.timeLeft - 1))
        end
        if s.ore >= b.obsidianCostOre && s.clay >= b.obsidianCostClay
            push!(queue, State(s.ore - b.obsidianCostOre + s.oreRobots, s.clay - b.obsidianCostClay + s.clayRobots, s.obsidian + s.obsidianRobots, s.geode + s.geodeRobots, s.oreRobots, s.clayRobots, s.obsidianRobots + 1, s.geodeRobots, s.timeLeft - 1))
        end
        if s.ore >= b.geodeCostOre && s.obsidian >= b.geodeCostObsidian
            push!(queue, State(s.ore - b.geodeCostOre + s.oreRobots, s.clay + s.clayRobots, s.obsidian - b.geodeCostObsidian + s.obsidianRobots, s.geode + s.geodeRobots, s.oreRobots, s.clayRobots, s.obsidianRobots, s.geodeRobots + 1, s.timeLeft - 1))
        end
    end
    return maxGeodes
end

function main()
    blueprints = []
    open("input.txt", "r") do file
        for line in eachline(file)
            push!(blueprints, parseBlueprint(line))
        end
    end
    init = State(0, 0, 0, 0, 1, 0, 0, 0, 32)
    prod = 1
    for b in blueprints[1:3]
        prod *= maxGeode(b, init)
    end
    println(prod)
end

main()
