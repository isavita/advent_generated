
using DataStructures

function is_valid(r::Int, c::Int, grid::Matrix{Char}, grid_size::Int)
    return 1 <= r <= grid_size && 1 <= c <= grid_size && grid[r, c] == '.'
end

function bfs(grid::Matrix{Char}, grid_size::Int)
    q = Queue{Tuple{Int, Int, Int}}()
    start_node = (1, 1)
    target_node = (grid_size, grid_size)

    if grid[start_node...] == '#'
        return -1
    end

    enqueue!(q, (start_node[1], start_node[2], 0))
    visited = Set{Tuple{Int, Int}}()
    push!(visited, start_node)

    while !isempty(q)
        r, c, dist = dequeue!(q)

        if (r, c) == target_node
            return dist
        end

        for (dr, dc) in [(0, 1), (0, -1), (1, 0), (-1, 0)]
            nr, nc = r + dr, c + dc
            if is_valid(nr, nc, grid, grid_size) && !((nr, nc) in visited)
                push!(visited, (nr, nc))
                enqueue!(q, (nr, nc, dist + 1))
            end
        end
    end
    return -1
end

function solve()
    byte_positions = Tuple{Int, Int}[]
    open("input.txt", "r") do f
        for line in eachline(f)
            parts = split(strip(line), ",")
            x = parse(Int, parts[1]) + 1 # Adjust to 1-based indexing
            y = parse(Int, parts[2]) + 1 # Adjust to 1-based indexing
            push!(byte_positions, (x, y))
        end
    end

    grid_size = 71

    # Part 1
    grid1 = fill('.', grid_size, grid_size)
    for i in 1:min(length(byte_positions), 1024)
        x, y = byte_positions[i]
        if 1 <= y <= grid_size && 1 <= x <= grid_size
            grid1[y, x] = '#'
        end
    end
    part1_result = bfs(grid1, grid_size)
    println(part1_result)

    # Part 2
    grid2 = fill('.', grid_size, grid_size)
    for (i, (x, y)) in enumerate(byte_positions)
         if 1 <= y <= grid_size && 1 <= x <= grid_size
            grid2[y, x] = '#'
         else
             continue # Skip if coordinate out of bounds
         end

        if bfs(grid2, grid_size) == -1
            println("$(x-1),$(y-1)") # Print original 0-based coordinate
            break
        end
    end
end

function main()
    solve()
end

main()
