
function main()
    % Read the initial password from input.txt
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        current_password = strtrim(fscanf(fid, '%s'));
        fclose(fid);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % Find the next valid password
    next_password = find_next_valid_password(current_password);

    % Print the output to standard output
    fprintf('%s\n', next_password);

    % For Part Two, find the password after the first one
    next_password_part2 = find_next_valid_password(next_password);
    fprintf('%s\n', next_password_part2);
end

function is_valid = is_password_valid(password)
    % Rule 1: Must contain at least one increasing straight of three letters
    has_straight = false;
    for i = 1:(length(password) - 2)
        if password(i) + 1 == password(i+1) && password(i+1) + 1 == password(i+2)
            has_straight = true;
            break;
        end
    end

    % Rule 2: Cannot contain the letters i, o, or l
    has_forbidden_chars = any(password == 'i') || any(password == 'o') || any(password == 'l');

    % Rule 3: Must contain at least two different, non-overlapping pairs of letters
    pairs = [];
    i = 1;
    while i <= length(password) - 1
        if password(i) == password(i+1)
            pairs = [pairs, password(i)];
            i = i + 2; % Skip the next character as it's part of the pair
        else
            i = i + 1;
        end
    end
    has_two_pairs = length(unique(pairs)) >= 2;

    is_valid = has_straight && ~has_forbidden_chars && has_two_pairs;
end

function incremented_password = increment_password(password)
    password_chars = char(password);
    n = length(password_chars);
    i = n;
    while i >= 1
        if password_chars(i) == 'z'
            password_chars(i) = 'a';
            i = i - 1;
        else
            password_chars(i) = password_chars(i) + 1;
            break;
        end
    end
    incremented_password = char(password_chars);
end

function next_valid = find_next_valid_password(start_password)
    current = start_password;
    while true
        current = increment_password(current);
        if is_password_valid(current)
            next_valid = current;
            break;
        end
    end
end

% Call the main function to start the program
main();
