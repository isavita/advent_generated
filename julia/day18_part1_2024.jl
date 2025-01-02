
function solve()
    grid = falses(71, 71)
    open("input.txt") do f
        for (i, line) in enumerate(eachline(f))
            if i > 1024
                break
            end
            x, y = parse.(Int, split(line, ","))
            if 1 <= x + 1 <= 71 && 1 <= y + 1 <= 71
                grid[y + 1, x + 1] = true
            end
        end
    end

    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    visited = falses(71, 71)
    q = [(1, 1, 0)]
    visited[1, 1] = true

    while !isempty(q)
        x, y, steps = popfirst!(q)
        if x == 71 && y == 71
            println(steps)
            return
        end
        for (dx, dy) in dirs
            nx, ny = x + dx, y + dy
            if 1 <= nx <= 71 && 1 <= ny <= 71 && !grid[ny, nx] && !visited[ny, nx]
                visited[ny, nx] = true
                push!(q, (nx, ny, steps + 1))
            end
        end
    end
    println("No path")
end

solve()
