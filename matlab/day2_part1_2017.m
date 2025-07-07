
function corruption_checksum()
% CORRUPTION_CHECKSUM Reads a spreadsheet from input.txt and calculates its checksum.
%   The checksum is the sum of the differences between the largest and smallest
%   values in each row.

    % Define the input filename
    input_filename = 'input.txt';

    % Initialize the total checksum
    total_checksum = 0;

    % Open the input file for reading
    try
        file_id = fopen(input_filename, 'r');
        if file_id == -1
            error('Unable to open file: %s', input_filename);
        end

        % Read the file line by line
        while ~feof(file_id)
            % Read a line from the file
            line = fgetl(file_id);

            % Skip empty lines
            if isempty(line)
                continue;
            end

            % Parse the numbers in the line, assuming space-separated integers
            % Using sscanf for efficient parsing of space-separated numbers
            row_numbers = sscanf(line, '%d');

            % Check if any numbers were successfully parsed
            if isempty(row_numbers)
                warning('Skipping line with no valid numbers: %s', line);
                continue;
            end

            % Calculate the difference between the maximum and minimum values in the row
            row_difference = max(row_numbers) - min(row_numbers);

            % Add the row difference to the total checksum
            total_checksum = total_checksum + row_difference;
        end

        % Close the input file
        fclose(file_id);

        % Print the final checksum to standard output
        fprintf('The spreadsheet checksum is: %d\n', total_checksum);

    catch ME
        % Handle potential errors during file processing
        fprintf(2, 'Error: %s\n', ME.message);
        % Ensure the file is closed even if an error occurs
        if exist('file_id', 'var') && file_id ~= -1
            fclose(file_id);
        end
    end
end

% --- Main entry point ---
% This allows the script to be run directly.
% If this file is saved as 'corruption_checksum.m', calling it from the
% MATLAB command window will execute the function.
% For example:
% >> corruption_checksum
