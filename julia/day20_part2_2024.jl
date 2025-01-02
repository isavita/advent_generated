
function solve()
    grid = readlines("input.txt")
    h, w = length(grid), length(grid[1])
    S = (0, 0)
    E = (0, 0)
    walls = fill(false, h, w)
    trackCells = []

    for i in 1:h
        for j in 1:w
            ch = grid[i][j]
            if ch == 'S'
                S = (i, j)
            elseif ch == 'E'
                E = (i, j)
            end
            if ch == '#'
                walls[i, j] = true
            else
                push!(trackCells, (i, j))
            end
        end
    end

    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    isTrack(x, y) = x >= 1 && x <= h && y >= 1 && y <= w && !walls[x, y]

    function normalDistFrom(start)
        dist = fill(-1, h, w)
        dist[start[1], start[2]] = 0
        q = [start]
        head = 1
        while head <= length(q)
            cur = q[head]
            head += 1
            for d in dirs
                nx, ny = cur[1] + d[1], cur[2] + d[2]
                if nx < 1 || nx > h || ny < 1 || ny > w
                    continue
                end
                if walls[nx, ny]
                    continue
                end
                if dist[nx, ny] < 0
                    dist[nx, ny] = dist[cur[1], cur[2]] + 1
                    push!(q, (nx, ny))
                end
            end
        end
        return dist
    end

    distFromS = normalDistFrom(S)
    distFromE = normalDistFrom(E)
    if distFromS[E[1], E[2]] < 0
        println(0)
        return
    end
    normalCost = distFromS[E[1], E[2]]

    cheats = Dict()

    for startPos in trackCells
        sd = distFromS[startPos[1], startPos[2]]
        if sd < 0
            continue
        end

        distC = fill(-1, h, w)
        distC[startPos[1], startPos[2]] = 0
        q = [startPos]
        head = 1

        while head <= length(q)
            cur = q[head]
            head += 1
            steps = distC[cur[1], cur[2]]
            if steps == 20
                continue
            end
            for d in dirs
                nx, ny = cur[1] + d[1], cur[2] + d[2]
                if nx < 1 || nx > h || ny < 1 || ny > w
                    continue
                end
                if distC[nx, ny] < 0
                    distC[nx, ny] = steps + 1
                    push!(q, (nx, ny))
                end
            end
        end

        for x in 1:h
            for y in 1:w
                s = distC[x, y]
                if s > 0 && s <= 20 && isTrack(x, y)
                    ed = distFromE[x, y]
                    if ed < 0
                        continue
                    end
                    cost = sd + s + ed
                    if cost < normalCost
                        key = (startPos[1], startPos[2], x, y)
                        if !haskey(cheats, key) || cost < cheats[key]
                            cheats[key] = cost
                        end
                    end
                end
            end
        end
    end

    count = 0
    for cost in values(cheats)
        saving = normalCost - cost
        if saving >= 100
            count += 1
        end
    end
    println(count)
end

solve()
