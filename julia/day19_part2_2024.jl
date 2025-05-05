
function count_ways(design, patterns)
    n = length(design)
    dp = zeros(Int, n + 1)
    dp[1] = 1

    for i in 1:n
        for p in patterns
            lp = length(p)
            if i >= lp
                if design[i - lp + 1:i] == p
                    dp[i + 1] += dp[i - lp + 1]
                end
            end
        end
    end
    return dp[n + 1]
end

function main()
    total_ways = 0
    open("input.txt", "r") do f
        line1 = readline(f)
        available_patterns = [strip(p) for p in split(line1, ',')]
        readline(f) # Skip second line

        while !eof(f)
            design = readline(f)
            design = strip(design)
            if !isempty(design)
                 total_ways += count_ways(design, available_patterns)
            end
        end
    end
    println(total_ways)
end

main()
