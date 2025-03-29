
using DataStructures

function read_input(file_path)
    walls = Set{CartesianIndex{2}}()
    blizzards = Vector{Tuple{Int, Int, Char}}()
    y = 0
    width = 0
    open(file_path, "r") do file
        for (iy, line) in enumerate(eachline(file))
            y = iy
            sline = strip(line)
            width = length(sline)
            for (x, char) in enumerate(sline)
                if char == '#'
                    push!(walls, CartesianIndex(x, y))
                elseif char in ['>', '<', '^', 'v']
                    push!(blizzards, (x, y, char))
                end
            end
        end
    end
    height = y
    return walls, blizzards, height, width
end

function find_start_end(walls, height, width)
    start_pos = CartesianIndex(0, 0)
    end_pos = CartesianIndex(0, 0)
    for x in 1:width
        if CartesianIndex(x, 1) ∉ walls
            start_pos = CartesianIndex(x, 1)
            break
        end
    end
    for x in 1:width
        if CartesianIndex(x, height) ∉ walls
            end_pos = CartesianIndex(x, height)
            break
        end
    end
    return start_pos, end_pos
end

function precompute_blizzards(blizzards, width, height, period)
    blizzard_positions = [Set{CartesianIndex{2}}() for _ in 1:period]
    inner_width = width - 2
    inner_height = height - 2

    for t in 0:(period-1)
        current_blizzards = Set{CartesianIndex{2}}()
        for (x, y, dir) in blizzards
            nx, ny = x, y
            if dir == '>'
                nx = 2 + mod(x - 2 + t, inner_width)
            elseif dir == '<'
                nx = 2 + mod(x - 2 - t, inner_width)
            elseif dir == 'v'
                ny = 2 + mod(y - 2 + t, inner_height)
            elseif dir == '^'
                ny = 2 + mod(y - 2 - t, inner_height)
            end
            push!(current_blizzards, CartesianIndex(nx, ny))
        end
        # Julia arrays are 1-based, map time t (0..period-1) to index (1..period)
        blizzard_positions[t + 1] = current_blizzards
    end
    return blizzard_positions
end

function bfs(start_pos, end_pos, walls, blizzard_positions, period, width, height, start_time)
    queue = Deque{Tuple{CartesianIndex{2}, Int}}()
    visited = Set{Tuple{CartesianIndex{2}, Int}}()

    push!(queue, (start_pos, start_time))
    push!(visited, (start_pos, mod(start_time, period)))

    directions = [
        CartesianIndex(0, 0),  # Wait
        CartesianIndex(1, 0),  # Right
        CartesianIndex(-1, 0), # Left
        CartesianIndex(0, 1),  # Down
        CartesianIndex(0, -1)  # Up
    ]

    while !isempty(queue)
        pos, t = popfirst!(queue)
        x, y = pos[1], pos[2]

        next_t = t + 1
        time_mod_period_next = mod(next_t, period)
        blizzards_next = blizzard_positions[time_mod_period_next + 1] # +1 for 1-based index

        for d in directions
            npos = pos + d
            nx, ny = npos[1], npos[2]

            # Check if reached the end
            if npos == end_pos
                return next_t
            end

            # Check boundaries and walls
            if ny < 1 || ny > height || nx < 1 || nx > width || npos in walls
                continue
            end

            # Check blizzards
            if npos in blizzards_next
                continue
            end

            state = (npos, time_mod_period_next)
            if state ∉ visited
                push!(visited, state)
                push!(queue, (npos, next_t))
            end
        end
    end

    return -1 # Should not happen based on problem description
end

function main()
    input_file = "input.txt"
    walls, blizzards, height, width = read_input(input_file)
    start_pos, end_pos = find_start_end(walls, height, width)

    # Period is LCM of inner dimensions
    period = lcm(width - 2, height - 2)
    blizzard_positions = precompute_blizzards(blizzards, width, height, period)

    # Trip 1: Start -> End
    time1 = bfs(start_pos, end_pos, walls, blizzard_positions, period, width, height, 0)
    if time1 == -1
        println("Error: No path found for trip 1.")
        return
    end

    # Trip 2: End -> Start
    time2 = bfs(end_pos, start_pos, walls, blizzard_positions, period, width, height, time1)
     if time2 == -1
        println("Error: No path found for trip 2.")
        return
    end

    # Trip 3: Start -> End
    time3 = bfs(start_pos, end_pos, walls, blizzard_positions, period, width, height, time2)
    if time3 == -1
        println("Error: No path found for trip 3.")
        return
    end

    println(time3)
end

main()
