
function parse_map(filename)
    lines = readlines(filename)
    return [collect(line) for line in lines]
end

function is_valid_move(grid, x, y, visited)
    height, width = length(grid), length(grid[1])
    return (1 <= x <= height && 
            1 <= y <= width && 
            grid[x][y] != '#' && 
            !visited[x, y])
end

function dfs(grid, x, y, end_x, end_y, visited, current_path)
    height, width = length(grid), length(grid[1])
    
    # Reached the end
    if x == end_x && y == end_y
        return length(current_path) - 1
    end
    
    # Mark current tile as visited
    visited[x, y] = true
    
    # Possible moves based on current tile
    moves = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    
    # Slope constraints
    if grid[x][y] == '>'
        moves = [(x, y+1)]
    elseif grid[x][y] == '<'
        moves = [(x, y-1)]
    elseif grid[x][y] == '^'
        moves = [(x-1, y)]
    elseif grid[x][y] == 'v'
        moves = [(x+1, y)]
    end
    
    max_path = 0
    for (nx, ny) in moves
        if is_valid_move(grid, nx, ny, visited)
            max_path = max(max_path, dfs(grid, nx, ny, end_x, end_y, copy(visited), [current_path; (nx, ny)]))
        end
    end
    
    return max_path
end

function solve_longest_hike(filename)
    grid = parse_map(filename)
    height, width = length(grid), length(grid[1])
    
    # Find start and end positions
    start_y = findfirst(x -> x == '.', grid[1])
    end_y = findfirst(x -> x == '.', grid[end])
    
    visited = zeros(Bool, height, width)
    
    return dfs(grid, 1, start_y, height, end_y, visited, [(1, start_y)])
end

# Read input and solve
println(solve_longest_hike("input.txt"))
