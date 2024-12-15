
function checkMAS(grid, x, y, dx, dy)
    rows, cols = size(grid)
    word = "MAS"
    forward = true
    backward = true
    for i in 1:3
        nx, ny = x + (dx * (i - 1)), y + (dy * (i - 1))
        if !(1 <= nx <= rows && 1 <= ny <= cols)
            forward = false
            break
        end
        if grid[nx, ny] != word[i]
            forward = false
        end
    end
    for i in 1:3
        nx, ny = x + (dx * (i - 1)), y + (dy * (i - 1))
        if !(1 <= nx <= rows && 1 <= ny <= cols)
            backward = false
            break
        end
        if grid[nx, ny] != word[4 - i]
            backward = false
        end
    end
    return forward || backward
end

function checkXMAS(grid, x, y)
    rows, cols = size(grid)
    if x < 2 || x > rows - 1 || y < 2 || y > cols - 1
        return false
    end
    return (checkMAS(grid, x - 1, y - 1, 1, 1) && checkMAS(grid, x - 1, y + 1, 1, -1)) ||
           (checkMAS(grid, x + 1, y - 1, -1, 1) && checkMAS(grid, x + 1, y + 1, -1, -1))
end

function countXMASPatterns(grid)
    rows, cols = size(grid)
    count = 0
    if rows < 3 || cols < 3
        return 0
    end
    for i in 2:rows-1
        for j in 2:cols-1
            if grid[i, j] == 'A' && checkXMAS(grid, i, j)
                count += 1
            end
        end
    end
    return count
end

function main()
    grid = open("input.txt") do file
        filter(!isempty, [strip(line) for line in eachline(file)])
    end
    grid = reduce(hcat, collect.(grid))
    count = countXMASPatterns(grid)
    println("X-MAS patterns appear $count times in the word search")
end

main()
