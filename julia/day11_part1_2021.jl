
function solve()
    grid = read_input("input.txt")
    total_flashes = 0
    
    for _ in 1:100
        total_flashes += simulate_step!(grid)
    end
    
    println(total_flashes)
end

function read_input(filename)
    grid = Int8[]
    open(filename) do file
        for line in eachline(file)
            append!(grid, parse.(Int8, collect(line)))
        end
    end
    return reshape(grid, 10, 10)
end

function simulate_step!(grid)
    flashes = 0
    flashed = falses(size(grid))
    
    # Increase energy by 1 for all octopuses
    grid .+= 1
    
    # Flash octopuses with energy greater than 9
    for y in 1:size(grid, 1), x in 1:size(grid, 2)
        if grid[y, x] > 9
            flashes += flash!(grid, x, y, flashed)
        end
    end
    
    # Reset energy to 0 for all that flashed
    grid[flashed] .= 0
    
    return flashes
end

function flash!(grid, x, y, flashed)
    if flashed[y, x]
        return 0
    end
    
    flashed[y, x] = true
    flashes = 1
    
    for dy in -1:1, dx in -1:1
        if dx == 0 && dy == 0
            continue
        end
        
        new_x, new_y = x + dx, y + dy
        
        if 1 <= new_x <= size(grid, 2) && 1 <= new_y <= size(grid, 1)
            grid[new_y, new_x] += 1
            if grid[new_y, new_x] > 9
                flashes += flash!(grid, new_x, new_y, flashed)
            end
        end
    end
    
    return flashes
end

solve()
