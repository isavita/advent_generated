
function solve()
    lines = readlines("input.txt")
    nr = length(lines)
    nc = length(lines[1])
    grid = zeros(Int, nr, nc)
    for i in 1:nr, j in 1:nc
        grid[i, j] = parse(Int, lines[i][j])
    end

    dp = fill(-1, nr, nc)
    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

    function dfs(r, c)
        if dp[r, c] != -1
            return dp[r, c]
        end
        h = grid[r, c]
        if h == 9
            dp[r, c] = 1
            return 1
        end
        sum = 0
        for (dr, dc) in dirs
            nr2, nc2 = r + dr, c + dc
            if nr2 < 1 || nr2 > nr || nc2 < 1 || nc2 > nc
                continue
            end
            if grid[nr2, nc2] == h + 1
                sum += dfs(nr2, nc2)
            end
        end
        dp[r, c] = sum
        return sum
    end

    total = 0
    for r in 1:nr, c in 1:nc
        if grid[r, c] == 0
            total += dfs(r, c)
        end
    end
    println(total)
end

solve()
