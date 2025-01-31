
local function solve()
    local f = io.open("input.txt", "r")
    local grid = {}
    for line in f:lines() do
        table.insert(grid, line)
    end
    f:close()

    local n, m = #grid, #grid[1]
    local sx, sy, ex, ey
    for i = 1, n do
        for j = 1, m do
            if grid[i]:sub(j, j) == 'S' then
                sx, sy = i, j
            elseif grid[i]:sub(j, j) == 'E' then
                ex, ey = i, j
            end
        end
    end

    local dx = {-1, 0, 1, 0}
    local dy = {0, 1, 0, -1}

    local dist = {}
    for i = 1, n do
        dist[i] = {}
        for j = 1, m do
            dist[i][j] = {math.huge, math.huge, math.huge, math.huge}
        end
    end
    dist[sx][sy][2] = 0

    local h = {}
    local function push(v)
        table.insert(h, v)
        local i = #h
        while i > 1 do
            local p = math.floor((i - 1) / 2) + 1
            if h[p][4] <= h[i][4] then
                break
            end
            h[p], h[i] = h[i], h[p]
            i = p
        end
    end

    local function pop()
        local v = h[1]
        h[1] = h[#h]
        h[#h] = nil
        local i = 1
        while true do
            local l = 2 * i
            local r = 2 * i + 1
            local small = i
            if l <= #h and h[l][4] < h[small][4] then
                small = l
            end
            if r <= #h and h[r][4] < h[small][4] then
                small = r
            end
            if small == i then
                break
            end
            h[i], h[small] = h[small], h[i]
            i = small
        end
        return v
    end

    push({sx, sy, 2, 0})

    while #h > 0 do
        local u = pop()
        if dist[u[1]][u[2]][u[3]] < u[4] then
            goto continue
        end
        if u[1] == ex and u[2] == ey then
            goto continue
        end
        for _, ndir in ipairs({(u[3] % 4) + 1, ((u[3] + 2) % 4) + 1}) do
            local nc = u[4] + 1000
            if nc < dist[u[1]][u[2]][ndir] then
                dist[u[1]][u[2]][ndir] = nc
                push({u[1], u[2], ndir, nc})
            end
        end
        local nx, ny = u[1] + dx[u[3]], u[2] + dy[u[3]]
        if nx >= 1 and nx <= n and ny >= 1 and ny <= m and grid[nx]:sub(ny, ny) ~= '#' then
            local nc = u[4] + 1
            if nc < dist[nx][ny][u[3]] then
                dist[nx][ny][u[3]] = nc
                push({nx, ny, u[3], nc})
            end
        end
        ::continue::
    end

    local best = math.huge
    for d = 1, 4 do
        if dist[ex][ey][d] < best then
            best = dist[ex][ey][d]
        end
    end

    local used = {}
    for i = 1, n do
        used[i] = {}
        for j = 1, m do
            used[i][j] = false
        end
    end

    local rev = {}
    for d = 1, 4 do
        if dist[ex][ey][d] == best then
            table.insert(rev, {ex, ey, d})
        end
    end

    local vis = {}
    for i = 1, n do
        vis[i] = {}
        for j = 1, m do
            vis[i][j] = {false, false, false, false}
        end
    end
    for _, s in ipairs(rev) do
        vis[s[1]][s[2]][s[3]] = true
    end

    while #rev > 0 do
        local u = rev[#rev]
        rev[#rev] = nil
        used[u[1]][u[2]] = true

        local costU = dist[u[1]][u[2]][u[3]]

        for _, pd in ipairs({(u[3] % 4) + 1, ((u[3] + 2) % 4) + 1}) do
            if dist[u[1]][u[2]][pd] == costU - 1000 then
                if not vis[u[1]][u[2]][pd] then
                    vis[u[1]][u[2]][pd] = true
                    table.insert(rev, {u[1], u[2], pd})
                end
            end
        end

        local px, py = u[1] - dx[u[3]], u[2] - dy[u[3]]
        if px >= 1 and px <= n and py >= 1 and py <= m and grid[px]:sub(py, py) ~= '#' then
            if dist[px][py][u[3]] == costU - 1 then
                if not vis[px][py][u[3]] then
                    vis[px][py][u[3]] = true
                    table.insert(rev, {px, py, u[3]})
                end
            end
        end
    end

    local cnt = 0
    for i = 1, n do
        for j = 1, m do
            if used[i][j] and grid[i]:sub(j, j) ~= '#' then
                cnt = cnt + 1
            end
        end
    end

    print(cnt)
end

solve()
