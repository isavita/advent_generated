
using DataStructures

function read_input(file_path::String)
    walls = Set{Tuple{Int, Int}}()
    blizzards = Vector{Tuple{Int, Int, Char}}()
    height = 0
    width = 0
    open(file_path, "r") do file
        for (y, line) in enumerate(eachline(file))
            height = y
            line = strip(line)
            width = length(line)
            for (x, char) in enumerate(line)
                if char == '#'
                    push!(walls, (x, y))
                elseif char in ['>', '<', '^', 'v']
                    push!(blizzards, (x, y, char))
                end
            end
        end
    end
    return walls, blizzards, height, width
end

function find_start_end(walls::Set{Tuple{Int, Int}}, height::Int, width::Int)
    start_pos = (0, 0)
    end_pos = (0, 0)
    for x in 1:width
        if !((x, 1) in walls)
            start_pos = (x, 1)
            break
        end
    end
    for x in 1:width
        if !((x, height) in walls)
            end_pos = (x, height)
            break
        end
    end
    return start_pos, end_pos
end

function compute_period(width::Int, height::Int)
    return lcm(width, height)
end

function precompute_blizzards(blizzards::Vector{Tuple{Int, Int, Char}}, width::Int, height::Int, period::Int)
    inner_width = width - 2
    inner_height = height - 2
    blizzard_positions = [Set{Tuple{Int, Int}}() for _ in 1:period]

    for t in 0:(period-1)
        current_blizzards = Set{Tuple{Int, Int}}()
        for (bx, by, dir) in blizzards
            nx, ny = bx, by
            if dir == '>'
                nx = 2 + mod(bx - 2 + t, inner_width)
            elseif dir == '<'
                nx = 2 + mod(bx - 2 - t, inner_width)
            elseif dir == 'v'
                ny = 2 + mod(by - 2 + t, inner_height)
            elseif dir == '^'
                ny = 2 + mod(by - 2 - t, inner_height)
            end
            push!(current_blizzards, (nx, ny))
        end
        blizzard_positions[t + 1] = current_blizzards # Store at 1-based index t+1
    end
    return blizzard_positions
end

function bfs(start_pos::Tuple{Int, Int}, end_pos::Tuple{Int, Int}, walls::Set{Tuple{Int, Int}},
             blizzard_positions::Vector{Set{Tuple{Int, Int}}}, period::Int, width::Int, height::Int)

    queue = Deque{Tuple{Int, Int, Int}}()
    visited = Set{Tuple{Int, Int, Int}}()

    push!(queue, (start_pos[1], start_pos[2], 0)) # x, y, time
    push!(visited, (start_pos[1], start_pos[2], 0)) # x, y, time_mod_period

    directions = [(0, 0), (1, 0), (-1, 0), (0, 1), (0, -1)] # Wait, Right, Left, Down, Up

    while !isempty(queue)
        x, y, t = popfirst!(queue)

        next_t = t + 1
        blizzards_next_time_index = mod(next_t, period) + 1 # 1-based index for blizzard positions
        blizzards_next = blizzard_positions[blizzards_next_time_index]

        for (dx, dy) in directions
            nx, ny = x + dx, y + dy

            if (nx, ny) == end_pos
                return next_t
            end

            # Check bounds (within walls) and not a wall itself
            if ny < 1 || ny > height || nx < 1 || nx > width || (nx, ny) in walls
                continue
            end

            # Check if occupied by a blizzard at next step
            if (nx, ny) in blizzards_next
                continue
            end

            state = (nx, ny, mod(next_t, period))
            if !(state in visited)
                push!(visited, state)
                push!(queue, (nx, ny, next_t))
            end
        end
    end
    return -1 # Should not happen given problem constraints
end

function main()
    input_file = "input.txt"
    walls, blizzards, height, width = read_input(input_file)
    start_pos, end_pos = find_start_end(walls, height, width)

    # Inner dimensions for period calculation
    inner_width = width - 2
    inner_height = height - 2
    period = compute_period(inner_width, inner_height)

    blizzard_positions = precompute_blizzards(blizzards, width, height, period)

    minutes = bfs(start_pos, end_pos, walls, blizzard_positions, period, width, height)
    println(minutes)
end

main()
