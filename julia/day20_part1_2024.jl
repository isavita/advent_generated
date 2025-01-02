
function solve()
    grid = readlines("input.txt")
    h = length(grid)
    w = length(grid[1])
    S = CartesianIndex(0, 0)
    E = CartesianIndex(0, 0)
    track_cells = CartesianIndex[]
    walls = falses(h, w)

    for i in 1:h, j in 1:w
        if grid[i][j] == 'S'
            S = CartesianIndex(i, j)
        elseif grid[i][j] == 'E'
            E = CartesianIndex(i, j)
        end
        if grid[i][j] == '#'
            walls[i, j] = true
        else
            push!(track_cells, CartesianIndex(i, j))
        end
    end

    dirs = [CartesianIndex(1, 0), CartesianIndex(-1, 0), CartesianIndex(0, 1), CartesianIndex(0, -1)]

    function bfs(start::CartesianIndex)
        dist = fill(-1, h, w)
        dist[start] = 0
        q = [start]
        while !isempty(q)
            cur = popfirst!(q)
            for d in dirs
                next = cur + d
                if checkbounds(Bool, walls, next) && !walls[next] && dist[next] == -1
                    dist[next] = dist[cur] + 1
                    push!(q, next)
                end
            end
        end
        return dist
    end

    dist_from_S = bfs(S)
    dist_from_E = bfs(E)

    if dist_from_S[E] == -1
        println(0)
        return
    end

    normal_cost = dist_from_S[E]

    is_track(x, y) = checkbounds(Bool, walls, x, y) && !walls[x, y]

    possible_cheats = 0
    for start_pos in track_cells
        sd = dist_from_S[start_pos]
        if sd == -1
            continue
        end
        for d1 in dirs
            m1 = start_pos + d1
            if !checkbounds(Bool, walls, m1)
                continue
            end
            for d2 in dirs
                m2 = m1 + d2
                if !checkbounds(Bool, walls, m2)
                    continue
                end
                if !is_track(m2.I...)
                    continue
                end
                ed = dist_from_E[m2]
                if ed == -1
                    continue
                end
                new_cost = sd + 2 + ed
                saving = normal_cost - new_cost
                if saving >= 100
                    possible_cheats += 1
                end
            end
        end
    end
    println(possible_cheats)
end

solve()
