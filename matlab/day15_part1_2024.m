
function main()
    global grid;
    [grid, moves] = read_input('input.txt');
    
    [rows, cols] = size(grid);
    [robot_r, robot_c] = find(grid == '@', 1);

    for i = 1:length(moves)
        dr = 0; dc = 0;
        switch moves(i)
            case '^', dr = -1;
            case 'v', dr = 1;
            case '<', dc = -1;
            case '>', dc = 1;
            otherwise, continue;
        end

        nr = robot_r + dr;
        nc = robot_c + dc;

        if nr < 1 || nr > rows || nc < 1 || nc > cols
            continue;
        end

        next_cell = grid(nr, nc);

        if next_cell == '#'
            continue;
        elseif next_cell == 'O'
            if ~push_box(nr, nc, dr, dc)
                continue;
            end
        end
        
        grid(robot_r, robot_c) = '.';
        grid(nr, nc) = '@';
        robot_r = nr;
        robot_c = nc;
    end

    [box_rows, box_cols] = find(grid == 'O');
    total_sum = sum((box_rows - 1) * 100 + (box_cols - 1));
    
    fprintf('%d\n', int64(total_sum));
end

function success = push_box(r, c, dr, dc)
    global grid;
    [rows, cols] = size(grid);

    nr = r + dr;
    nc = c + dc;

    if nr < 1 || nr > rows || nc < 1 || nc > cols
        success = false;
        return;
    end

    next_cell = grid(nr, nc);

    if next_cell == '#'
        success = false;
        return;
    end

    if next_cell == 'O'
        if ~push_box(nr, nc, dr, dc)
            success = false;
            return;
        end
    end
    
    grid(nr, nc) = 'O';
    grid(r, c) = '.';
    success = true;
end

function [grid, moves] = read_input(filename)
    fid = fopen(filename, 'r');
    
    grid_lines = {};
    moves_str = '';
    reading_map = true;

    while ~feof(fid)
        line = fgetl(fid);
        if ~ischar(line), break; end
        
        if isempty(strtrim(line))
            if ~isempty(grid_lines)
                reading_map = false;
            end
            continue;
        end

        if reading_map
            grid_lines{end+1} = line;
        else
            moves_str = [moves_str, strtrim(line)];
        end
    end
    fclose(fid);

    grid = char(grid_lines);
    moves = moves_str;
end
