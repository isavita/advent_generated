
Clean = 0;
Weakened = 1;
Infected = 2;
Flagged = 3;

MAX_DIM_OFFSET = 10000;
GRID_DIM = 2 * MAX_DIM_OFFSET;
grid = zeros(GRID_DIM, GRID_DIM, 'uint8');
offset = MAX_DIM_OFFSET + 1;

fid = fopen('input.txt', 'r');
lines = textscan(fid, '%s', 'Delimiter', '\n');
fclose(fid);
lines = lines{1};

num_rows_input = length(lines);
num_cols_input = length(lines{1});

startX_input = floor(num_cols_input / 2);
startY_input = floor(num_rows_input / 2);

for y_rel = 0:num_rows_input-1
    line = lines{y_rel+1};
    for x_rel = 0:num_cols_input-1
        if line(x_rel+1) == '#'
            x_global = x_rel - startX_input;
            y_global = y_rel - startY_input;
            grid(y_global + offset, x_global + offset) = Infected;
        end
    end
end

dx = [0, 1, 0, -1];
dy = [-1, 0, 1, 0];

x = 0;
y = 0;
dir = 1; % 1:Up, 2:Right, 3:Down, 4:Left

infectedCount = 0;
num_iterations = 10000000;

for i = 1:num_iterations
    current_state = grid(y + offset, x + offset);
    
    switch current_state
        case Clean
            dir = mod(dir - 1 - 1 + 4, 4) + 1;
            grid(y + offset, x + offset) = Weakened;
        case Weakened
            grid(y + offset, x + offset) = Infected;
            infectedCount = infectedCount + 1;
        case Infected
            dir = mod(dir - 1 + 1, 4) + 1;
            grid(y + offset, x + offset) = Flagged;
        case Flagged
            dir = mod(dir - 1 + 2, 4) + 1;
            grid(y + offset, x + offset) = Clean;
    end
    
    x = x + dx(dir);
    y = y + dy(dir);
end

fprintf('%d\n', infectedCount);
