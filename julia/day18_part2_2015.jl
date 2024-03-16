const GRID_SIZE = 100
const STEPS = 100

function count_on_neighbors(grid, x, y)
    on = 0
    for dx in -1:1, dy in -1:1
        if dx == 0 && dy == 0
            continue
        end
        nx, ny = x + dx, y + dy
        if nx >= 1 && nx <= GRID_SIZE && ny >= 1 && ny <= GRID_SIZE && grid[nx, ny]
            on += 1
        end
    end
    return on
end

function step(grid)
    new_grid = falses(GRID_SIZE, GRID_SIZE)
    for x in 1:GRID_SIZE, y in 1:GRID_SIZE
        on_neighbors = count_on_neighbors(grid, x, y)
        if grid[x, y]
            new_grid[x, y] = on_neighbors == 2 || on_neighbors == 3
        else
            new_grid[x, y] = on_neighbors == 3
        end
    end
    new_grid[1, 1] = true
    new_grid[1, GRID_SIZE] = true
    new_grid[GRID_SIZE, 1] = true
    new_grid[GRID_SIZE, GRID_SIZE] = true
    return new_grid
end

function main()
    grid = falses(GRID_SIZE, GRID_SIZE)
    open("input.txt", "r") do file
        y = 1
        for line in eachline(file)
            for x in 1:length(line)
                grid[x, y] = line[x] == '#'
            end
            y += 1
        end
    end
    grid[1, 1] = true
    grid[1, GRID_SIZE] = true
    grid[GRID_SIZE, 1] = true
    grid[GRID_SIZE, GRID_SIZE] = true

    for i in 1:STEPS
        grid = step(grid)
    end

    on_count = 0
    for x in 1:GRID_SIZE, y in 1:GRID_SIZE
        if grid[x, y]
            on_count += 1
        end
    end
    println(on_count)
end

main()