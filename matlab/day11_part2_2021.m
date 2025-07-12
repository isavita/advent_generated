
function main()
    global grid;
    global SIZE;
    SIZE = 10;

    readInput('input.txt');

    step = 0;
    while true
        step = step + 1;
        num_flashes = simulateStep();
        if num_flashes == SIZE * SIZE
            break;
        end
    end

    fprintf('%d\n', step);
end

function readInput(filename)
    global grid;
    global SIZE;

    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file %s', filename);
    end
    
    grid = zeros(SIZE, SIZE);
    for i = 1:SIZE
        line = fgetl(fid);
        for j = 1:SIZE
            grid(i, j) = str2double(line(j));
        end
    end
    fclose(fid);
end

function [flashes, flashed] = flash(x, y, flashed_in)
    global grid;
    global SIZE;
    
    flashed = flashed_in;
    
    if flashed(y, x)
        flashes = 0;
        return;
    end
    
    flashed(y, x) = 1;
    flashes = 1;
    
    % Directions (dy, dx) for 1-indexed grid
    % C: directions[i][0] is dx, directions[i][1] is dy
    % MATLAB: dy = directions(k,1), dx = directions(k,2)
    directions = [
        -1, -1; % dy, dx for C's {-1, -1}
         0, -1; % dy, dx for C's {-1, 0}
         1, -1; % dy, dx for C's {-1, 1}
        -1,  0; % dy, dx for C's {0, -1}
         1,  0; % dy, dx for C's {0, 1}
        -1,  1; % dy, dx for C's {1, -1}
         0,  1; % dy, dx for C's {1, 0}
         1,  1  % dy, dx for C's {1, 1}
    ];
    
    for k = 1:size(directions, 1)
        dy = directions(k, 1);
        dx = directions(k, 2);
        
        newY = y + dy;
        newX = x + dx;
        
        if newX >= 1 && newX <= SIZE && newY >= 1 && newY <= SIZE
            grid(newY, newX) = grid(newY, newX) + 1;
            if grid(newY, newX) > 9
                [sub_flashes, flashed] = flash(newX, newY, flashed);
                flashes = flashes + sub_flashes;
            end
        end
    end
end

function total_flashes = simulateStep()
    global grid;
    global SIZE;
    
    total_flashes = 0;
    flashed = zeros(SIZE, SIZE);
    
    grid = grid + 1;
    
    for y = 1:SIZE
        for x = 1:SIZE
            if grid(y, x) > 9
                [current_flashes, flashed] = flash(x, y, flashed);
                total_flashes = total_flashes + current_flashes;
            end
        end
    end
    
    grid(flashed == 1) = 0;
end
