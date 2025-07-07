
function main()
    % Read input from input.txt
    try
        file_id = fopen('input.txt', 'r');
        if file_id == -1
            error('Could not open input.txt for reading.');
        end
        image_data = textscan(file_id, '%s', 'Delimiter', '\n');
        fclose(file_id);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % Convert cell array of strings to a character array
    image = char(image_data{1});

    % --- Part 1: Expansion Factor 2 ---
    expansion_factor_part1 = 2;
    total_distance_part1 = calculate_total_galaxy_distance(image, expansion_factor_part1);
    fprintf('Part 1: Sum of shortest path lengths (expansion factor 2): %d\n', total_distance_part1);

    % --- Part 2: Expansion Factor 1,000,000 ---
    expansion_factor_part2 = 1000000;
    total_distance_part2 = calculate_total_galaxy_distance(image, expansion_factor_part2);
    fprintf('Part 2: Sum of shortest path lengths (expansion factor 1,000,000): %d\n', total_distance_part2);
end

function total_distance = calculate_total_galaxy_distance(image, expansion_factor)
    % Find empty rows and columns
    num_rows = size(image, 1);
    num_cols = size(image, 2);

    empty_rows = true(num_rows, 1);
    empty_cols = true(1, num_cols);

    galaxies = [];
    for r = 1:num_rows
        for c = 1:num_cols
            if image(r, c) == '#'
                empty_rows(r) = false;
                empty_cols(c) = false;
                galaxies = [galaxies; [r, c]]; %#ok<AGROW>
            end
        end
    end

    % Calculate expanded coordinates
    expanded_galaxies = zeros(size(galaxies, 1), 2);
    current_row_offset = 0;
    for r = 1:num_rows
        expanded_galaxies(empty_rows(r), 1) = r + current_row_offset;
        if empty_rows(r)
            current_row_offset = current_row_offset + (expansion_factor - 1);
        end
    end

    current_col_offset = 0;
    for c = 1:num_cols
        expanded_galaxies(empty_cols(c), 2) = c + current_col_offset;
        if empty_cols(c)
            current_col_offset = current_col_offset + (expansion_factor - 1);
        end
    end

    % Adjust galaxy coordinates based on expansion
    for i = 1:size(galaxies, 1)
        original_row = galaxies(i, 1);
        original_col = galaxies(i, 2);

        row_expansion = sum(empty_rows(1:original_row-1)) * (expansion_factor - 1);
        col_expansion = sum(empty_cols(1:original_col-1)) * (expansion_factor - 1);

        expanded_galaxies(i, 1) = original_row + row_expansion;
        expanded_galaxies(i, 2) = original_col + col_expansion;
    end

    % Calculate sum of distances between all pairs of galaxies
    total_distance = 0;
    num_galaxies = size(expanded_galaxies, 1);
    for i = 1:num_galaxies
        for j = i + 1:num_galaxies
            dist = abs(expanded_galaxies(i, 1) - expanded_galaxies(j, 1)) + ...
                   abs(expanded_galaxies(i, 2) - expanded_galaxies(j, 2));
            total_distance = total_distance + dist;
        end
    end
end

% Call the main function to execute the program
if nargin == 0
    main();
end
