
function main()
    % Read input from input.txt
    try
        initial_state = fileread('input.txt');
        % Remove any trailing newline characters
        initial_state = strtrim(initial_state);
    catch
        fprintf(2, 'Error: Could not read from input.txt.\n');
        return;
    end

    % Define the target disk size
    target_disk_size = 272; % As per the problem description

    % Generate the data to fill the disk
    generated_data = generate_dragon_data(initial_state, target_disk_size);

    % Calculate the checksum
    checksum = calculate_checksum(generated_data);

    % Print the checksum to standard output
    fprintf('%s\n', checksum);
end

function data = generate_dragon_data(initial_state, target_size)
    % Generates data using the modified dragon curve until target_size is met.
    data = initial_state;
    while length(data) < target_size
        a = data;
        b = a;
        % Reverse b
        b = b(end:-1:1);
        % Invert bits in b
        b = strrep(strrep(b, '0', '#'), '1', '0');
        b = strrep(b, '#', '1');
        % Concatenate
        data = [a, '0', b];
    end
    % Truncate to the target size
    data = data(1:target_size);
end

function checksum = calculate_checksum(data)
    % Calculates the checksum of the given data.
    current_checksum = data;
    while mod(length(current_checksum), 2) == 0
        next_checksum = '';
        for i = 1:2:length(current_checksum) - 1
            pair = current_checksum(i:i+1);
            if pair(1) == pair(2)
                next_checksum = [next_checksum, '1'];
            else
                next_checksum = [next_checksum, '0'];
            end
        end
        current_checksum = next_checksum;
    end
    checksum = current_checksum;
end

% Call the main function to execute the program
main();
