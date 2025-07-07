
function total_flashes = solve_dumbo_octopus()
    % SOLVE_DUMBO_OCTOPUS Reads input from input.txt and simulates
    % the Dumbo Octopus problem for 100 steps, returning the total flashes.

    % --- Configuration ---
    NUM_STEPS = 100;
    GRID_SIZE = 10;

    % --- Read Input ---
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        grid_str = textscan(fid, '%s');
        fclose(fid);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        total_flashes = -1; % Indicate an error
        return;
    end

    % Convert string grid to numeric array
    octopus_grid = zeros(GRID_SIZE, GRID_SIZE);
    for i = 1:GRID_SIZE
        row_str = grid_str{1}{i};
        for j = 1:GRID_SIZE
            octopus_grid(i, j) = str2double(row_str(j));
        end
    end

    % --- Simulation ---
    total_flashes = 0;
    for step = 1:NUM_STEPS
        % 1. Energy level of each octopus increases by 1.
        octopus_grid = octopus_grid + 1;

        % Keep track of octopuses that have flashed in this step
        has_flashed = false(GRID_SIZE, GRID_SIZE);
        flashed_in_this_step = true; % Flag to continue propagation

        while flashed_in_this_step
            flashed_in_this_step = false;
            
            % Find octopuses with energy > 9 that haven't flashed yet
            [flash_row, flash_col] = find(octopus_grid > 9 & ~has_flashed);

            if ~isempty(flash_row)
                flashed_in_this_step = true;
                
                % Mark these octopuses as flashed for this step
                for k = 1:length(flash_row)
                    has_flashed(flash_row(k), flash_col(k)) = true;
                end

                % Increase energy of adjacent octopuses
                for k = 1:length(flash_row)
                    r = flash_row(k);
                    c = flash_col(k);

                    % Iterate over all 8 neighbors (and the octopus itself, though it's handled by has_flashed)
                    for dr = -1:1
                        for dc = -1:1
                            nr = r + dr;
                            nc = c + dc;

                            % Check if neighbor is within grid bounds
                            if nr >= 1 && nr <= GRID_SIZE && nc >= 1 && nc <= GRID_SIZE
                                % Only increase energy if the neighbor hasn't flashed yet in this step
                                if ~has_flashed(nr, nc)
                                    octopus_grid(nr, nc) = octopus_grid(nr, nc) + 1;
                                end
                            end
                        end
                    end
                end
            end
        end

        % 3. Set flashed octopuses' energy to 0 and count flashes.
        flashes_this_step = sum(has_flashed(:));
        total_flashes = total_flashes + flashes_this_step;
        octopus_grid(has_flashed) = 0;
    end

    % --- Output ---
    fprintf('Total flashes after %d steps: %d\n', NUM_STEPS, total_flashes);
end

% --- Main Entry Point ---
% This function is called when the script is run directly.
function main()
    solve_dumbo_octopus();
end

% Call the main function if the script is run directly
if isempty(ver('matlab')) || ~isdeployed()
    main();
end
