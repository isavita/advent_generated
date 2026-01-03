
function main()
    lines = readlines("input.txt")
    isempty(lines) && return

    rows = length(lines)
    cols = maximum(length.(lines))

    grid = fill(' ', rows, cols)
    for (i, line) in enumerate(lines)
        for (j, c) in enumerate(line)
            grid[i, j] = c
        end
    end

    total = 0
    j = 1
    while j <= cols
        all(i -> grid[i, j] == ' ', 1:rows) && (j += 1; continue)

        start = j
        while j <= cols && !all(i -> grid[i, j] == ' ', 1:rows)
            j += 1
        end
        total += solve(grid, start, j - 1)
    end
    println(total)
end

function solve(grid, start, finish)
    rows, op = size(grid, 1), '+'
    for c in start:finish
        x = grid[rows, c]
        if x == '+' || x == '*'
            op = x
            break
        end
    end

    nums = Int[]
    for c in finish:-1:start
        s = strip(String(grid[1:rows-1, c]))
        !isempty(s) && push!(nums, parse(Int, s))
    end

    isempty(nums) && return 0
    op == '+' ? sum(nums) : prod(nums)
end

main()
