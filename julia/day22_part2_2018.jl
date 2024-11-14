
using DataStructures

# Enum for tools and region types
@enum Tool torch=1 climbing_gear=2 neither=3
@enum RegionType rocky=0 wet=1 narrow=2

mutable struct State
    x::Int
    y::Int
    tool::Tool
    time::Int
end

function calculate_geologic_index(x::Int, y::Int, target_x::Int, target_y::Int, depth::Int, cache::Dict)
    # Check cache first
    key = (x, y)
    if haskey(cache, key)
        return cache[key]
    end

    # Calculate geologic index based on rules
    geologic_index = if x == 0 && y == 0
        0
    elseif x == target_x && y == target_y
        0
    elseif y == 0
        x * 16807
    elseif x == 0
        y * 48271
    else
        erosion_level_left = calculate_erosion_level(x-1, y, target_x, target_y, depth, cache)
        erosion_level_up = calculate_erosion_level(x, y-1, target_x, target_y, depth, cache)
        erosion_level_left * erosion_level_up
    end

    # Store in cache
    cache[key] = geologic_index
    return geologic_index
end

function calculate_erosion_level(x::Int, y::Int, target_x::Int, target_y::Int, depth::Int, cache::Dict)
    geologic_index = calculate_geologic_index(x, y, target_x, target_y, depth, cache)
    return (geologic_index + depth) % 20183
end

function get_region_type(x::Int, y::Int, target_x::Int, target_y::Int, depth::Int, cache::Dict)
    erosion_level = calculate_erosion_level(x, y, target_x, target_y, depth, cache)
    return RegionType(erosion_level % 3)
end

function valid_tool(region_type::RegionType, tool::Tool)
    if region_type == rocky
        return tool == torch || tool == climbing_gear
    elseif region_type == wet
        return tool == climbing_gear || tool == neither
    elseif region_type == narrow
        return tool == torch || tool == neither
    end
end

function shortest_path(target_x::Int, target_y::Int, depth::Int)
    cache = Dict{Tuple{Int,Int}, Int}()
    visited = Set{Tuple{Int,Int,Tool}}()
    queue = PriorityQueue{State, Int}()

    start = State(0, 0, torch, 0)
    enqueue!(queue, start, start.time)

    while !isempty(queue)
        current = dequeue!(queue)

        # Check if reached target
        if current.x == target_x && current.y == target_y && current.tool == torch
            return current.time
        end

        # Skip if already visited with same state
        state_key = (current.x, current.y, current.tool)
        if state_key ∈ visited
            continue
        end
        push!(visited, state_key)

        # Try changing tools
        current_region = get_region_type(current.x, current.y, target_x, target_y, depth, cache)
        for new_tool in instances(Tool)
            if new_tool != current.tool && valid_tool(current_region, new_tool)
                new_state = State(current.x, current.y, new_tool, current.time + 7)
                if (current.x, current.y, new_tool) ∉ visited
                    enqueue!(queue, new_state, new_state.time)
                end
            end
        end

        # Try moving to adjacent regions
        for (dx, dy) in [(0,1), (0,-1), (1,0), (-1,0)]
            new_x, new_y = current.x + dx, current.y + dy
            
            # Skip invalid regions
            if new_x < 0 || new_y < 0
                continue
            end

            new_region = get_region_type(new_x, new_y, target_x, target_y, depth, cache)
            if valid_tool(new_region, current.tool)
                new_state = State(new_x, new_y, current.tool, current.time + 1)
                if (new_x, new_y, current.tool) ∉ visited
                    enqueue!(queue, new_state, new_state.time)
                end
            end
        end
    end
end

function main()
    # Read input from file
    lines = readlines("input.txt")
    depth = parse(Int, split(lines[1], ": ")[2])
    target_coords = split(lines[2], ": ")[2]
    target_x, target_y = parse.(Int, split(target_coords, ","))

    # Part 1: Calculate total risk level
    risk_level = 0
    cache = Dict{Tuple{Int,Int}, Int}()
    for x in 0:target_x, y in 0:target_y
        region_type = get_region_type(x, y, target_x, target_y, depth, cache)
        risk_level += Int(region_type)
    end
    println("Part 1: Total Risk Level = ", risk_level)

    # Part 2: Find shortest path
    time_to_target = shortest_path(target_x, target_y, depth)
    println("Part 2: Fewest minutes to reach target = ", time_to_target)
end

main()
