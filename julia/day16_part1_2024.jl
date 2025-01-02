
function solve()
    grid = readlines("input.txt")
    n, m = length(grid), length(grid[1])
    sx, sy, ex, ey = 0, 0, 0, 0
    for i in 1:n, j in 1:m
        if grid[i][j] == 'S'
            sx, sy = i, j
        elseif grid[i][j] == 'E'
            ex, ey = i, j
        end
    end
    dx = [-1, 0, 1, 0]
    dy = [0, 1, 0, -1]
    dist = fill(typemax(Int), n, m, 4)
    dist[sx, sy, 2] = 0
    pq = [(sx, sy, 2, 0)]
    while !isempty(pq)
        x, y, d, cost = popfirst!(pq)
        if dist[x, y, d] < cost
            continue
        end
        if x == ex && y == ey
            println(cost)
            return
        end
        for ndir in [(d + 1) % 4, (d + 3) % 4]
            ndir == 0 && (ndir = 4)
            nc = cost + 1000
            if nc < dist[x, y, ndir]
                dist[x, y, ndir] = nc
                push!(pq, (x, y, ndir, nc))
            end
        end
        nx, ny = x + dx[d], y + dy[d]
        if 1 <= nx <= n && 1 <= ny <= m && grid[nx][ny] != '#'
            nc = cost + 1
            if nc < dist[nx, ny, d]
                dist[nx, ny, d] = nc
                push!(pq, (nx, ny, d, nc))
            end
        end
        sort!(pq, by = x -> x[4])
    end
end

solve()
