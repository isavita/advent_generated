
function solve()
    lines = readlines("input.txt")
    available_patterns = split(strip(lines[1]), ",")
    available_patterns = strip.(available_patterns)
    count = 0
    for design in lines[3:end]
        if can_make(design, available_patterns)
            count += 1
        end
    end
    println(count)
end

function can_make(design, patterns)
    n = length(design)
    dp = falses(n + 1)
    dp[1] = true
    for i in 1:n
        if !dp[i]
            continue
        end
        for p in patterns
            lp = length(p)
            if i + lp <= n + 1 && design[i:i+lp-1] == p
                dp[i+lp] = true
            end
        end
    end
    return dp[n+1]
end

solve()
