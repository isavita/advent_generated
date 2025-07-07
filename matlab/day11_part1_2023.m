
function main()
    % Read input from input.txt
    try
        input_data = fileread('input.txt');
    catch
        error('Error: Could not read input.txt. Make sure the file exists in the current directory.');
    end

    % Process the input data
    lines = strsplit(input_data, '\n');
    lines = lines(~cellfun('isempty', lines)); % Remove empty lines

    % Find empty rows and columns
    num_rows = length(lines);
    num_cols = length(lines{1});

    empty_rows = true(1, num_rows);
    empty_cols = true(1, num_cols);

    galaxies = [];
    galaxy_count = 0;

    for r = 1:num_rows
        for c = 1:num_cols
            if lines{r}(c) == '#'
                empty_rows(r) = false;
                empty_cols(c) = false;
                galaxy_count = galaxy_count + 1;
                galaxies(galaxy_count, :) = [r, c];
            end
        end
    end

    % Define expansion factor (twice as big means adding 1 for each empty row/col)
    expansion_factor = 1; % For the original problem, this is 1 (doubling means adding 1)

    % Calculate expanded coordinates
    expanded_galaxies = zeros(size(galaxies));
    row_offset = 0;
    for r = 1:num_rows
        if empty_rows(r)
            row_offset = row_offset + expansion_factor;
        end
        expanded_galaxies(galaxies(:, 1) == r, 1) = r + row_offset;
    end

    col_offset = 0;
    for c = 1:num_cols
        if empty_cols(c)
            col_offset = col_offset + expansion_factor;
        end
        expanded_galaxies(galaxies(:, 2) == c, 2) = galaxies(galaxies(:, 2) == c, 2) + col_offset;
    end

    % Calculate sum of shortest path lengths
    total_path_length = 0;
    for i = 1:galaxy_count
        for j = i + 1:galaxy_count
            % Manhattan distance in the expanded universe
            distance = abs(expanded_galaxies(i, 1) - expanded_galaxies(j, 1)) + ...
                       abs(expanded_galaxies(i, 2) - expanded_galaxies(j, 2));
            total_path_length = total_path_length + distance;
        end
    end

    % Print the output
    fprintf('Sum of shortest path lengths: %d\n', total_path_length);
end

% This is the main entry point for the script.
% When the script is run, this function will be called.
if nargin == 0
    main();
end
