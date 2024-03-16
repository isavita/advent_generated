open("input.txt") do file
    lines = readlines(file)
    m, n = length(lines), length(lines[1])
    grid = [lines[i][j] for i in 1:m, j in 1:n]
    steps = 0
    while true
        moved = false
        new_grid = copy(grid)
        for i in 1:m, j in 1:n
            if grid[i,j] == '>'
                if grid[i,(j%n)+1] == '.'
                    new_grid[i,(j%n)+1] = '>'
                    new_grid[i,j] = '.'
                    moved = true
                end
            end
        end
        grid = new_grid
        new_grid = copy(grid)
        for i in 1:m, j in 1:n
            if grid[i,j] == 'v'
                if grid[(i%m)+1,j] == '.'
                    new_grid[(i%m)+1,j] = 'v'
                    new_grid[i,j] = '.'
                    moved = true
                end
            end
        end
        grid = new_grid
        steps += 1
        if !moved
            println(steps)
            break
        end
    end
end