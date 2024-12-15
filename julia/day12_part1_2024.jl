
function solve()
    grid = readlines("input.txt")
    rows = length(grid)
    rows == 0 && return 0
    cols = length(grid[1])
    visited = zeros(Bool, rows, cols)
    total_price = 0
    for r in 1:rows, c in 1:cols
        if !visited[r, c]
            area, perimeter = calculate_region(grid, r, c, visited)
            total_price += area * perimeter
        end
    end
    return total_price
end

function calculate_region(grid, row, col, visited)
    rows = length(grid)
    cols = length(grid[1])
    char = grid[row][col]
    area = 0
    perimeter = 0
    queue = [(row, col)]
    visited[row, col] = true
    while !isempty(queue)
        p = popfirst!(queue)
        area += 1
        is_border = p[1] == 1 || p[1] == rows || p[2] == 1 || p[2] == cols
        for (dr, dc) in [(-1, 0), (1, 0), (0, -1), (0, 1)]
            nr, nc = p[1] + dr, p[2] + dc
            if 1 <= nr <= rows && 1 <= nc <= cols
                if grid[nr][nc] != char
                    perimeter += 1
                elseif !visited[nr, nc]
                    push!(queue, (nr, nc))
                    visited[nr, nc] = true
                end
            elseif is_border
                perimeter += 1
            end
        end
    end
    return area, perimeter
end

println(solve())
