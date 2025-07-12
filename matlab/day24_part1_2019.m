
function main()
    GRID_SIZE = 5;

    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end
    
    initial_grid = char(zeros(GRID_SIZE, GRID_SIZE));
    for i = 1:GRID_SIZE
        line = fgetl(fid);
        initial_grid(i, :) = line(1:GRID_SIZE);
    end
    fclose(fid);

    current_grid = initial_grid;
    
    seen_state_strings = {}; 
    initial_grid_string = reshape(initial_grid', 1, []); 
    seen_state_strings{1} = initial_grid_string;
    
    while true
        next_grid = updateState(current_grid);
        next_grid_string = reshape(next_grid', 1, []);

        if ismember(next_grid_string, seen_state_strings)
            biodiversity = calculateBiodiversity(next_grid);
            fprintf('%d\n', biodiversity);
            break;
        end
        
        seen_state_strings{end+1} = next_grid_string;
        current_grid = next_grid;
    end
end

function next_grid = updateState(current_grid)
    GRID_SIZE = 5;
    numeric_grid = (current_grid == '#'); 

    kernel = [0 1 0; 1 0 1; 0 1 0]; 
    adj_counts = conv2(double(numeric_grid), kernel, 'same');

    current_is_bug = (current_grid == '#');
    next_is_bug_logical = false(GRID_SIZE, GRID_SIZE);

    next_is_bug_logical(current_is_bug) = (adj_counts(current_is_bug) == 1);
    next_is_bug_logical(~current_is_bug) = (adj_counts(~current_is_bug) == 1 | adj_counts(~current_is_bug) == 2);

    next_grid = repmat('.', GRID_SIZE, GRID_SIZE);
    next_grid(next_is_bug_logical) = '#';
end

function biodiversity = calculateBiodiversity(grid)
    GRID_SIZE = 5;
    
    bugs_logical_row_major = reshape((grid == '#')', 1, []); 
    
    powers = 2.^(0:GRID_SIZE*GRID_SIZE-1);
    
    biodiversity = sum(powers(bugs_logical_row_major));
end
