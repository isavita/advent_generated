
function solve()
    lines = readlines("input.txt")
    nr = length(lines)
    nc = length(lines[1])
    grid = zeros(Int, nr, nc)
    for i in 1:nr, j in 1:nc
        grid[i, j] = parse(Int, lines[i][j])
    end

    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    trailheads = Tuple{Int, Int}[]
    for r in 1:nr, c in 1:nc
        if grid[r, c] == 0
            push!(trailheads, (r, c))
        end
    end

    sum_scores = 0
    for th in trailheads
        reached = Set{Tuple{Int, Int}}()
        front = [(th, 0)]
        visited = Set{Tuple{Int, Int, Int}}()
        while !isempty(front)
            (cur_p, cur_h) = pop!(front)
            if cur_h == 9
                push!(reached, cur_p)
                continue
            end
            for (dr, dc) in dirs
                nr2, nc2 = cur_p[1] + dr, cur_p[2] + dc
                if nr2 < 1 || nr2 > nr || nc2 < 1 || nc2 > nc
                    continue
                end
                if grid[nr2, nc2] == cur_h + 1
                    key = (nr2, nc2, cur_h + 1)
                    if !(key in visited)
                        push!(visited, key)
                        push!(front, ((nr2, nc2), cur_h + 1))
                    end
                end
            end
        end
        sum_scores += length(reached)
    end
    println(sum_scores)
end

solve()
