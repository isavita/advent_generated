
local function solve()
    local f = io.open("input.txt", "r")
    local grid = {}
    for line in f:lines() do
        table.insert(grid, line)
    end
    f:close()

    local h = #grid
    local w = #grid[1]
    local S, E = {x=0,y=0}, {x=0,y=0}
    local trackCells = {}
    local walls = {}
    for i = 1, h do
        walls[i] = {}
        for j = 1, w do
            walls[i][j] = false
        end
    end

    for i = 1, h do
        for j = 1, w do
            local ch = grid[i]:sub(j, j)
            if ch == 'S' then
                S.x, S.y = i, j
            elseif ch == 'E' then
                E.x, E.y = i, j
            end
            if ch == '#' then
                walls[i][j] = true
            else
                table.insert(trackCells, {x = i, y = j})
            end
        end
    end

    local dirs = {{x = 1, y = 0}, {x = -1, y = 0}, {x = 0, y = 1}, {x = 0, y = -1}}

    local function normalDistFrom(start)
        local dist = {}
        for i = 1, h do
            dist[i] = {}
            for j = 1, w do
                dist[i][j] = -1
            end
        end
        dist[start.x][start.y] = 0
        local q = {start}
        local head = 1
        while head <= #q do
            local cur = q[head]
            head = head + 1
            for _, d in ipairs(dirs) do
                local nx, ny = cur.x + d.x, cur.y + d.y
                if nx >= 1 and nx <= h and ny >= 1 and ny <= w and not walls[nx][ny] and dist[nx][ny] == -1 then
                    dist[nx][ny] = dist[cur.x][cur.y] + 1
                    table.insert(q, {x = nx, y = ny})
                end
            end
        end
        return dist
    end

    local distFromS = normalDistFrom(S)
    local distFromE = normalDistFrom(E)

    if distFromS[E.x][E.y] == -1 then
        print(0)
        return
    end

    local normalCost = distFromS[E.x][E.y]

    local function isTrack(x, y)
        return x >= 1 and x <= h and y >= 1 and y <= w and not walls[x][y]
    end

    local possibleCheats = 0

    for _, startPos in ipairs(trackCells) do
        local sd = distFromS[startPos.x][startPos.y]
        if sd ~= -1 then
            for _, d1 in ipairs(dirs) do
                local m1 = {x = startPos.x + d1.x, y = startPos.y + d1.y}
                if m1.x >= 1 and m1.x <= h and m1.y >= 1 and m1.y <= w then
                    for _, d2 in ipairs(dirs) do
                        local m2 = {x = m1.x + d2.x, y = m1.y + d2.y}
                        if m2.x >= 1 and m2.x <= h and m2.y >= 1 and m2.y <= w and isTrack(m2.x, m2.y) then
                            local ed = distFromE[m2.x][m2.y]
                            if ed ~= -1 then
                                local newCost = sd + 2 + ed
                                local saving = normalCost - newCost
                                if saving >= 100 then
                                    possibleCheats = possibleCheats + 1
                                end
                            end
                        end
                    end
                end
            end
        end
    end

    print(possibleCheats)
end

solve()
