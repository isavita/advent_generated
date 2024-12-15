
function solve()
    grid = readlines("input.txt")
    h = length(grid)
    w = length(grid[1])
    grid = [collect(row) for row in grid]

    start_x, start_y, start_dir = 0, 0, 0
    for i in 1:h, j in 1:w
        if grid[i][j] == '^'
            start_x, start_y, start_dir = j, i, 1
        elseif grid[i][j] == '>'
            start_x, start_y, start_dir = j, i, 2
        elseif grid[i][j] == 'v'
            start_x, start_y, start_dir = j, i, 3
        elseif grid[i][j] == '<'
            start_x, start_y, start_dir = j, i, 4
        end
    end
    grid[start_y][start_x] = '.'

    can_loop = 0
    for y in 1:h, x in 1:w
        if x == start_x && y == start_y
            continue
        end
        if grid[y][x] != '.'
            continue
        end
        grid[y][x] = '#'
        if loops(grid, start_x, start_y, start_dir, h, w)
            can_loop += 1
        end
        grid[y][x] = '.'
    end
    println(can_loop)
end

function loops(grid, sx, sy, sdir, h, w)
    dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]
    x, y, dir = sx, sy, sdir
    seen = Set{Tuple{Int, Int, Int}}()
    for _ in 1:2000000
        st = (x, y, dir)
        if st in seen
            return true
        end
        push!(seen, st)
        dx, dy = dirs[dir]
        nx, ny = x + dx, y + dy
        if nx < 1 || nx > w || ny < 1 || ny > h
            return false
        end
        if grid[ny][nx] == '#'
            dir = mod1(dir + 1, 4)
            continue
        end
        x, y = nx, ny
    end
    return false
end

solve()
