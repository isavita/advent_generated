
function solve()
    grid = readlines("input.txt")
    h = length(grid)
    w = length(grid[1])
    dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]
    start_x, start_y, dir_idx = 0, 0, 0
    for y in 1:h, x in 1:w
        c = grid[y][x]
        if c == '^'
            start_x, start_y, dir_idx = x, y, 1
            break
        elseif c == '>'
            start_x, start_y, dir_idx = x, y, 2
            break
        elseif c == 'v'
            start_x, start_y, dir_idx = x, y, 3
            break
        elseif c == '<'
            start_x, start_y, dir_idx = x, y, 4
            break
        end
    end
    x, y = start_x, start_y
    visited = Set{Tuple{Int, Int}}()
    push!(visited, (x, y))
    while true
        dir_x, dir_y = dirs[dir_idx]
        nx, ny = x + dir_x, y + dir_y
        if nx < 1 || nx > w || ny < 1 || ny > h
            break
        end
        if grid[ny][nx] == '#'
            dir_idx = (dir_idx % 4) + 1
            continue
        end
        x, y = nx, ny
        push!(visited, (x, y))
    end
    println(length(visited))
end

solve()
