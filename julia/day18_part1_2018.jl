
function parse_input(filename)
    lines = readlines(filename)
    grid = [collect(line) for line in lines]
    return grid
end

function count_adjacent(grid, row, col)
    rows, cols = length(grid), length(grid[1])
    open_count = tree_count = lumber_count = 0
    
    for dx in -1:1, dy in -1:1
        if dx == 0 && dy == 0
            continue
        end
        
        new_row, new_col = row + dx, col + dy
        
        if 1 <= new_row <= rows && 1 <= new_col <= cols
            acre = grid[new_row][new_col]
            if acre == '.'
                open_count += 1
            elseif acre == '|'
                tree_count += 1
            elseif acre == '#'
                lumber_count += 1
            end
        end
    end
    
    return open_count, tree_count, lumber_count
end

function simulate_minute!(grid)
    rows, cols = length(grid), length(grid[1])
    new_grid = deepcopy(grid)
    
    for row in 1:rows, col in 1:cols
        open_count, tree_count, lumber_count = count_adjacent(grid, row, col)
        
        if grid[row][col] == '.'
            if tree_count >= 3
                new_grid[row][col] = '|'
            end
        elseif grid[row][col] == '|'
            if lumber_count >= 3
                new_grid[row][col] = '#'
            end
        elseif grid[row][col] == '#'
            if lumber_count == 0 || tree_count == 0
                new_grid[row][col] = '.'
            end
        end
    end
    
    return new_grid
end

function calculate_resource_value(grid)
    tree_count = sum(acre == '|' for row in grid for acre in row)
    lumber_count = sum(acre == '#' for row in grid for acre in row)
    return tree_count * lumber_count
end

function solve(filename, minutes)
    grid = parse_input(filename)
    
    for _ in 1:minutes
        grid = simulate_minute!(grid)
    end
    
    return calculate_resource_value(grid)
end

# Main execution
result = solve("input.txt", 10)
println("Total resource value after 10 minutes: ", result)
