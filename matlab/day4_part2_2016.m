
function main()
    % Read input from input.txt
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        room_data = textscan(fid, '%s', 'Delimiter', '\n');
        fclose(fid);
        room_data = room_data{1};
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    total_sector_id_sum = 0;
    north_pole_sector_id = -1;

    % Process each room
    for i = 1:length(room_data)
        room_str = room_data{i};

        % Parse room string
        [encrypted_name, sector_id_str, checksum] = parse_room_string(room_str);

        if isempty(encrypted_name) || isempty(sector_id_str) || isempty(checksum)
            fprintf(2, 'Warning: Skipping malformed room string: %s\n', room_str);
            continue;
        end

        sector_id = str2double(sector_id_str);
        if isnan(sector_id)
            fprintf(2, 'Warning: Skipping room with invalid sector ID: %s\n', room_str);
            continue;
        end

        % Check if the room is real
        if is_real_room(encrypted_name, checksum)
            total_sector_id_sum = total_sector_id_sum + sector_id;

            % Decrypt the room name for Part Two
            decrypted_name = decrypt_room_name(encrypted_name, sector_id);

            % Check if this is the North Pole objects room
            if contains(decrypted_name, 'northpoleobject')
                north_pole_sector_id = sector_id;
            end
        end
    end

    % Print output to standard output
    fprintf('Sum of sector IDs of real rooms: %d\n', total_sector_id_sum);
    if north_pole_sector_id ~= -1
        fprintf('Sector ID of the North Pole objects room: %d\n', north_pole_sector_id);
    else
        fprintf('North Pole objects room not found.\n');
    end
end

function [encrypted_name, sector_id_str, checksum] = parse_room_string(room_str)
    % Parses a room string into its components.
    % Returns:
    %   encrypted_name: The encrypted name part (e.g., 'aaaaa-bbb-z-y-x')
    %   sector_id_str: The sector ID as a string (e.g., '123')
    %   checksum: The checksum as a string (e.g., 'abxyz')

    % Find the last hyphen to separate the sector ID and checksum
    last_hyphen_idx = find(room_str == '-', 1, 'last');
    if isempty(last_hyphen_idx)
        encrypted_name = ''; sector_id_str = ''; checksum = '';
        return;
    end

    % Extract the part before the last hyphen
    name_and_sector_part = room_str(1:last_hyphen_idx-1);

    % Find the second to last hyphen to separate the encrypted name and sector ID
    second_last_hyphen_idx = find(name_and_sector_part == '-', 1, 'last');
    if isempty(second_last_hyphen_idx)
        encrypted_name = ''; sector_id_str = ''; checksum = '';
        return;
    end

    encrypted_name = name_and_sector_part(1:second_last_hyphen_idx-1);
    sector_id_str = name_and_sector_part(second_last_hyphen_idx+1:end);

    % Extract the checksum from the square brackets
    open_bracket_idx = find(room_str == '[', 1, 'last');
    close_bracket_idx = find(room_str == ']', 1, 'last');

    if isempty(open_bracket_idx) || isempty(close_bracket_idx) || open_bracket_idx >= close_bracket_idx
        checksum = '';
        return;
    end

    checksum = room_str(open_bracket_idx+1:close_bracket_idx-1);
end

function is_real = is_real_room(encrypted_name, checksum)
    % Determines if a room is real based on its encrypted name and checksum.
    %
    % Args:
    %   encrypted_name: The encrypted name string (e.g., 'aaaaa-bbb-z-y-x').
    %   checksum: The provided checksum string (e.g., 'abxyz').
    %
    % Returns:
    %   true if the room is real, false otherwise.

    % Remove dashes from the encrypted name to count character frequencies
    name_chars = encrypted_name(encrypted_name ~= '-');

    % Count character frequencies
    unique_chars = unique(name_chars);
    counts = zeros(size(unique_chars));
    for i = 1:length(unique_chars)
        counts(i) = sum(name_chars == unique_chars(i));
    end

    % Sort characters by frequency (descending) and then alphabetically (ascending)
    % Create a table for sorting
    char_table = table(unique_chars', counts', 'VariableNames', {'Char', 'Count'});

    % Sort: first by Count (descending), then by Char (ascending)
    sorted_table = sortrows(char_table, {'Count', 'Char'}, {'descend', 'ascend'});

    % Get the top 5 characters from the sorted list
    if size(sorted_table, 1) < 5
        calculated_checksum = sorted_table.Char;
    else
        calculated_checksum = sorted_table.Char(1:5);
    end

    % Compare with the provided checksum
    is_real = isequal(calculated_checksum, checksum);
end

function decrypted_name = decrypt_room_name(encrypted_name, sector_id)
    % Decrypts the room name using a Caesar cipher shift.
    %
    % Args:
    %   encrypted_name: The encrypted name string (e.g., 'qzmt-zixmtkozy-ivhz').
    %   sector_id: The sector ID, which determines the shift amount.
    %
    % Returns:
    %   The decrypted name string (e.g., 'very-encrypted-name').

    shift = mod(sector_id, 26); % Ensure shift is within alphabet range
    decrypted_chars = [];

    for char_code = double(encrypted_name)
        if char_code >= double('a') && char_code <= double('z')
            % Shift lowercase letters
            shifted_code = mod(char_code - double('a') - shift, 26) + double('a');
            decrypted_chars = [decrypted_chars, char(shifted_code)];
        elseif char_code == double('-')
            % Replace dashes with spaces
            decrypted_chars = [decrypted_chars, ' '];
        else
            % Keep other characters as they are (though problem statement implies only letters and dashes)
            decrypted_chars = [decrypted_chars, char(char_code)];
        end
    end
    decrypted_name = char(decrypted_chars);
end

% Entry point for the program
if nargin == 0
    main();
end
