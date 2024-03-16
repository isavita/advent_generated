struct Point
    x::Int
    y::Int
end

struct DoorMap
    doors::Dict{Point, Dict{Point, Bool}}
end

function build_map(regex::String)
    dm = DoorMap(Dict{Point, Dict{Point, Bool}}())
    stack = []
    cp = Point(0, 0)
    for c in regex
        if c == '('
            push!(stack, cp)
        elseif c == '|'
            cp = stack[end]
        elseif c == ')'
            cp = stack[end]
            pop!(stack)
        else
            np = move(cp, c)
            if !haskey(dm.doors, cp)
                dm.doors[cp] = Dict{Point, Bool}()
            end
            dm.doors[cp][np] = true
            cp = np
        end
    end
    return dm
end

function move(p::Point, dir::Char)
    if dir == 'N'
        return Point(p.x, p.y - 1)
    elseif dir == 'S'
        return Point(p.x, p.y + 1)
    elseif dir == 'E'
        return Point(p.x + 1, p.y)
    elseif dir == 'W'
        return Point(p.x - 1, p.y)
    end
    return p
end

function find_furthest_room(dm::DoorMap)
    visited = Dict{Point, Int}()
    visited[Point(0, 0)] = 0  # Initialize the starting point
    queue = [Point(0, 0)]
    max_doors = 0

    while !isempty(queue)
        p = popfirst!(queue)
        if haskey(dm.doors, p)
            for np in keys(dm.doors[p])
                if !haskey(visited, np)
                    visited[np] = visited[p] + 1
                    max_doors = max(max_doors, visited[np])
                    push!(queue, np)
                end
            end
        end
    end

    return max_doors
end

function max(a::Int, b::Int)
    return a > b ? a : b
end

# Read input from "input.txt"
input_data = read("input.txt", String)
regex = input_data[2:end-1]

# Build the door map and find the furthest room
dm = build_map(regex)
max_doors = find_furthest_room(dm)

# Print the answer
println(max_doors)