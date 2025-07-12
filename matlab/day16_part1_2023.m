
function solve_puzzle()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end
    lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    
    grid_data_cell = lines{1};
    numRows = length(grid_data_cell);
    if numRows == 0
        disp(0);
        return;
    end
    numCols = length(grid_data_cell{1});
    
    grid_data = char(zeros(numRows, numCols));
    for i = 1:numRows
        grid_data(i, :) = grid_data_cell{i};
    end
    
    start_beam.origin = [0, 0];
    start_beam.dir = [1, 0];
    
    result = calculatePropagation(grid_data, start_beam);
    disp(result);
end

function rotated_coord = rotate90(coord)
    rotated_coord = [coord(2), -coord(1)];
end

function rotated_coord = rotateNeg90(coord)
    rotated_coord = [-coord(2), coord(1)];
end

function in_bounds = isInBounds(coord_0_indexed, grid_height, grid_width)
    x = coord_0_indexed(1);
    y = coord_0_indexed(2);
    in_bounds = (x >= 0 && x < grid_width && y >= 0 && y < grid_height);
end

function next_beams = nextBeam(grid_data, beam)
    next_beams = {};
    
    x = beam.origin(1);
    y = beam.origin(2);
    
    [grid_height, grid_width] = size(grid_data);
    
    if ~isInBounds(beam.origin, grid_height, grid_width)
        return; 
    end
    
    char_at_pos = grid_data(y + 1, x + 1); 
    
    if char_at_pos == '.'
        next_beams{end+1} = struct('origin', [beam.origin(1) + beam.dir(1), beam.origin(2) + beam.dir(2)], 'dir', beam.dir);
        return;
    end
    
    switch char_at_pos
        case '/'
            if beam.dir(1) == 0
                new_dir = rotateNeg90(beam.dir);
            else
                new_dir = rotate90(beam.dir);
            end
            next_beams{end+1} = struct('origin', [beam.origin(1) + new_dir(1), beam.origin(2) + new_dir(2)], 'dir', new_dir);
            
        case '\'
            if beam.dir(1) == 0
                new_dir = rotate90(beam.dir);
            else
                new_dir = rotateNeg90(beam.dir);
            end
            next_beams{end+1} = struct('origin', [beam.origin(1) + new_dir(1), beam.origin(2) + new_dir(2)], 'dir', new_dir);
            
        case '|'
            if beam.dir(1) ~= 0
                dir1 = rotate90(beam.dir);
                dir2 = rotateNeg90(beam.dir);
                next_beams{end+1} = struct('origin', [beam.origin(1) + dir1(1), beam.origin(2) + dir1(2)], 'dir', dir1);
                next_beams{end+1} = struct('origin', [beam.origin(1) + dir2(1), beam.origin(2) + dir2(2)], 'dir', dir2);
            else
                next_beams{end+1} = struct('origin', [beam.origin(1) + beam.dir(1), beam.origin(2) + beam.dir(2)], 'dir', beam.dir);
            end
            
        case '-'
            if beam.dir(2) ~= 0
                dir1 = rotate90(beam.dir);
                dir2 = rotateNeg90(beam.dir);
                next_beams{end+1} = struct('origin', [beam.origin(1) + dir1(1), beam.origin(2) + dir1(2)], 'dir', dir1);
                next_beams{end+1} = struct('origin', [beam.origin(1) + dir2(1), beam.origin(2) + dir2(2)], 'dir', dir2);
            else
                next_beams{end+1} = struct('origin', [beam.origin(1) + beam.dir(1), beam.origin(2) + beam.dir(2)], 'dir', beam.dir);
            end
    end
end

function energized_count = calculatePropagation(grid_data, start_beam)
    [grid_height, grid_width] = size(grid_data);
    
    dirs_map = containers.Map();
    dirs_map('0,-1') = 1;
    dirs_map('1,0') = 2;
    dirs_map('0,1') = 3;
    dirs_map('-1,0') = 4;
    
    visited = false(grid_height, grid_width, 4);
    energized = false(grid_height, grid_width);
    energized_count = 0;
    
    to_explore = {start_beam}; 
    
    while ~isempty(to_explore)
        current_beam = to_explore{end};
        to_explore(end) = [];
        
        curr_x = current_beam.origin(1);
        curr_y = current_beam.origin(2);
        
        if ~isInBounds([curr_x, curr_y], grid_height, grid_width)
            continue;
        end
        
        curr_x_1based = curr_x + 1;
        curr_y_1based = curr_y + 1;
        
        dir_key = sprintf('%d,%d', current_beam.dir(1), current_beam.dir(2));
        dir_idx = dirs_map(dir_key);
        
        if visited(curr_y_1based, curr_x_1based, dir_idx)
            continue;
        end
        
        visited(curr_y_1based, curr_x_1based, dir_idx) = true;
        
        if ~energized(curr_y_1based, curr_x_1based)
            energized(curr_y_1based, curr_x_1based) = true;
            energized_count = energized_count + 1;
        end
        
        new_beams = nextBeam(grid_data, current_beam);
        
        for i = 1:length(new_beams)
            to_explore{end+1} = new_beams{i};
        end
    end
end
