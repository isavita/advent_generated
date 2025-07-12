
function main()
    grid = readInput('input.txt');
    [height, width] = size(grid);
    step = 0;
    while true
        [grid, eastMoved] = moveEast(grid, height, width);
        [grid, southMoved] = moveSouth(grid, height, width);
        step = step + 1;
        if ~eastMoved && ~southMoved
            break;
        end
    end
    fprintf('%d\n', step);
end

function [grid, moved] = moveEast(grid, height, width)
    moved = false;
    newGrid = grid;
    for y = 1:height
        for x = 1:width
            if grid(y, x) == '>'
                nextX = mod(x, width) + 1;
                if grid(y, nextX) == '.'
                    newGrid(y, nextX) = '>';
                    newGrid(y, x) = '.';
                    moved = true;
                end
            end
        end
    end
    grid = newGrid;
end

function [grid, moved] = moveSouth(grid, height, width)
    moved = false;
    newGrid = grid;
    for x = 1:width
        for y = 1:height
            if grid(y, x) == 'v'
                nextY = mod(y, height) + 1;
                if grid(nextY, x) == '.'
                    newGrid(nextY, x) = 'v';
                    newGrid(y, x) = '.';
                    moved = true;
                end
            end
        end
    end
    grid = newGrid;
end

function grid = readInput(filename)
    fid = fopen(filename, 'r');
    if fid == -1
        error('Error opening file');
    end
    grid = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    grid = cell2mat(grid{1});
end

main();
