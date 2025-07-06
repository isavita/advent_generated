
function main()
    gridSize = 100;
    steps = 100;

    grid = false(gridSize, gridSize);
    file = fopen("input.txt", "r");
    y = 1;
    while ~feof(file)
        line = strtrim(fgets(file));
        for x = 1:length(line)
            if line(x) == '#'
                grid(x, y) = true;
            end
        end
        y = y + 1;
    end
    fclose(file);

    grid(1, 1) = true;
    grid(1, gridSize) = true;
    grid(gridSize, 1) = true;
    grid(gridSize, gridSize) = true;

    for i = 1:steps
        newGrid = grid;
        for x = 1:gridSize
            for y = 1:gridSize
                onNeighbors = countOnNeighbors(grid, x, y, gridSize);
                if grid(x, y)
                    newGrid(x, y) = onNeighbors == 2 || onNeighbors == 3;
                else
                    newGrid(x, y) = onNeighbors == 3;
                end
            end
        end
        newGrid(1, 1) = true;
        newGrid(1, gridSize) = true;
        newGrid(gridSize, 1) = true;
        newGrid(gridSize, gridSize) = true;
        grid = newGrid;
    end

    onCount = sum(grid(:));
    fprintf("%d\n", onCount);
end

function on = countOnNeighbors(grid, x, y, gridSize)
    on = 0;
    for dx = -1:1
        for dy = -1:1
            if dx == 0 && dy == 0
                continue;
            end
            nx = x + dx;
            ny = y + dy;
            if nx >= 1 && nx <= gridSize && ny >= 1 && ny <= gridSize && grid(nx, ny)
                on = on + 1;
            end
        end
    end
end

main();
