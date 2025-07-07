
function main()
    % Read snailfish numbers from input.txt
    snailfish_numbers = read_snailfish_numbers('input.txt');

    % Initialize the sum with the first snailfish number
    current_sum = snailfish_numbers{1};

    % Add the remaining snailfish numbers
    for i = 2:length(snailfish_numbers)
        current_sum = add_snailfish(current_sum, snailfish_numbers{i});
        current_sum = reduce_snailfish(current_sum);
    end

    % Calculate and print the magnitude of the final sum
    magnitude = calculate_magnitude(current_sum);
    fprintf('The magnitude of the final sum is: %d\n', magnitude);
end

function snailfish_numbers = read_snailfish_numbers(filename)
    % Reads snailfish numbers from a file, parsing them into nested cell arrays.
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end

    snailfish_numbers = {};
    line = fgetl(fid);
    while ischar(line)
        snailfish_numbers{end+1} = parse_snailfish(line);
        line = fgetl(fid);
    end
    fclose(fid);
end

function parsed_snailfish = parse_snailfish(str)
    % Parses a string representation of a snailfish number into a nested cell array.
    % This is a recursive parsing function.
    if isempty(str)
        error('Empty string encountered during parsing.');
    end

    if str(1) == '['
        % Find the matching closing bracket for the outer pair.
        balance = 0;
        split_index = -1;
        for i = 1:length(str)
            if str(i) == '['
                balance = balance + 1;
            elseif str(i) == ']'
                balance = balance - 1;
            elseif str(i) == ',' && balance == 1
                split_index = i;
                break;
            end
        end

        if split_index == -1
            error('Mismatched brackets or invalid format: %s', str);
        end

        left_str = str(2:split_index-1);
        right_str = str(split_index+1:end-1);

        parsed_snailfish = {parse_snailfish(left_str), parse_snailfish(right_str)};
    else
        % It's a regular number.
        parsed_snailfish = str2double(str);
    end
end

function added_snailfish = add_snailfish(left, right)
    % Adds two snailfish numbers by forming a new pair.
    added_snailfish = {left, right};
end

function reduced_snailfish = reduce_snailfish(snailfish_num)
    % Repeatedly applies explosion and splitting rules until the snailfish number is reduced.
    current_num = snailfish_num;
    while true
        [exploded_num, exploded] = explode_snailfish(current_num);
        if exploded
            current_num = exploded_num;
            continue; % Restart the reduction process after an explosion
        end

        [split_num, split_occurred] = split_snailfish(current_num);
        if split_occurred
            current_num = split_num;
            continue; % Restart the reduction process after a split
        end

        % If neither explosion nor split occurred, the number is reduced.
        break;
    end
    reduced_snailfish = current_num;
end

function [exploded_num, exploded] = explode_snailfish(snailfish_num)
    % Explodes the leftmost pair nested inside four or more pairs.
    % Returns the modified snailfish number and a boolean indicating if an explosion occurred.
    [exploded_num, exploded, ~, ~] = explode_recursive(snailfish_num, 0);
end

function [modified_num, exploded, left_add, right_add] = explode_recursive(num, depth)
    % Recursive helper function for explosion.
    % Returns:
    %   modified_num: The snailfish number after potential explosion.
    %   exploded: Boolean indicating if an explosion happened in this call or its children.
    %   left_add: The value to be added to the left neighbor.
    %   right_add: The value to be added to the right neighbor.

    modified_num = num;
    exploded = false;
    left_add = 0;
    right_add = 0;

    if iscell(num)
        % If it's a pair, recurse into its elements.
        if depth == 4
            % This pair needs to explode.
            left_val = num{1};
            right_val = num{2};

            if ~isnumeric(left_val) || ~isnumeric(right_val)
                error('Exploding pair must contain two regular numbers.');
            end

            % The exploding pair is replaced by 0.
            modified_num = 0;
            exploded = true;
            left_add = left_val;
            right_add = right_val;
        else
            % Recurse into the left element.
            [left_modified, exploded_left, left_add_from_left, right_add_from_left] = explode_recursive(num{1}, depth + 1);
            modified_num{1} = left_modified;

            if exploded_left
                exploded = true;
                % If the left element exploded, its right value needs to be added to the right element of the current pair.
                if isnumeric(modified_num{2})
                    modified_num{2} = modified_num{2} + right_add_from_left;
                else
                    % Find the leftmost regular number in the right element and add to it.
                    [modified_right_element, added_to_right] = add_to_leftmost_regular(modified_num{2}, right_add_from_left);
                    modified_num{2} = modified_right_element;
                    if ~added_to_right
                        % If no regular number was found to add to, the right_add_from_left is passed up.
                        right_add = right_add_from_left;
                    end
                end
                % The left_add_from_left is passed up to the parent.
                left_add = left_add_from_left;
            else
                % If the left element did not explode, pass its left_add up.
                left_add = left_add_from_left;
                % If the left element had a right_add, add it to the current pair's right element.
                if right_add_from_left > 0
                    if isnumeric(modified_num{2})
                        modified_num{2} = modified_num{2} + right_add_from_left;
                    else
                        [modified_right_element, added_to_right] = add_to_leftmost_regular(modified_num{2}, right_add_from_left);
                        modified_num{2} = modified_right_element;
                        if ~added_to_right
                            right_add = right_add_from_left;
                        end
                    end
                end
            end

            % If an explosion has already occurred in the left subtree, we don't need to check the right subtree for explosion.
            if exploded
                return;
            end

            % Recurse into the right element.
            [right_modified, exploded_right, left_add_from_right, right_add_from_right] = explode_recursive(num{2}, depth + 1);
            modified_num{2} = right_modified;

            if exploded_right
                exploded = true;
                % If the right element exploded, its left value needs to be added to the left element of the current pair.
                if isnumeric(modified_num{1})
                    modified_num{1} = modified_num{1} + left_add_from_right;
                else
                    % Find the rightmost regular number in the left element and add to it.
                    [modified_left_element, added_to_left] = add_to_rightmost_regular(modified_num{1}, left_add_from_right);
                    modified_num{1} = modified_left_element;
                    if ~added_to_left
                        % If no regular number was found to add to, the left_add_from_right is passed up.
                        left_add = left_add_from_right;
                    end
                end
                % The right_add_from_right is passed up to the parent.
                right_add = right_add_from_right;
            else
                % If the right element did not explode, pass its right_add up.
                right_add = right_add_from_right;
                % If the right element had a left_add, add it to the current pair's left element.
                if left_add_from_right > 0
                    if isnumeric(modified_num{1})
                        modified_num{1} = modified_num{1} + left_add_from_right;
                    else
                        [modified_left_element, added_to_left] = add_to_rightmost_regular(modified_num{1}, left_add_from_right);
                        modified_num{1} = modified_left_element;
                        if ~added_to_left
                            left_add = left_add_from_right;
                        end
                    end
                end
            end
        end
    end
end

function [modified_num, added] = add_to_leftmost_regular(num, value_to_add)
    % Helper to add a value to the leftmost regular number in a snailfish structure.
    modified_num = num;
    added = false;
    if isnumeric(num)
        modified_num = num + value_to_add;
        added = true;
    elseif iscell(num)
        [modified_num{1}, added_left] = add_to_leftmost_regular(num{1}, value_to_add);
        if added_left
            added = true;
        else
            [modified_num{2}, added_right] = add_to_leftmost_regular(num{2}, value_to_add);
            if added_right
                added = true;
            end
        end
    end
end

function [modified_num, added] = add_to_rightmost_regular(num, value_to_add)
    % Helper to add a value to the rightmost regular number in a snailfish structure.
    modified_num = num;
    added = false;
    if isnumeric(num)
        modified_num = num + value_to_add;
        added = true;
    elseif iscell(num)
        [modified_num{2}, added_right] = add_to_rightmost_regular(num{2}, value_to_add);
        if added_right
            added = true;
        else
            [modified_num{1}, added_left] = add_to_rightmost_regular(num{1}, value_to_add);
            if added_left
                added = true;
            end
        end
    end
end


function [split_num, split_occurred] = split_snailfish(snailfish_num)
    % Splits the leftmost regular number that is 10 or greater.
    % Returns the modified snailfish number and a boolean indicating if a split occurred.
    [split_num, split_occurred] = split_recursive(snailfish_num);
end

function [modified_num, split_occurred] = split_recursive(num)
    % Recursive helper function for splitting.
    modified_num = num;
    split_occurred = false;

    if iscell(num)
        % Recurse into the left element.
        [left_modified, split_left] = split_recursive(num{1});
        modified_num{1} = left_modified;
        if split_left
            split_occurred = true;
            return; % Only one split per step
        end

        % Recurse into the right element.
        [right_modified, split_right] = split_recursive(num{2});
        modified_num{2} = right_modified;
        if split_right
            split_occurred = true;
            return; % Only one split per step
        end
    elseif isnumeric(num)
        % If it's a regular number and it's >= 10, split it.
        if num >= 10
            left_part = floor(num / 2);
            right_part = ceil(num / 2);
            modified_num = {left_part, right_part};
            split_occurred = true;
        end
    end
end

function magnitude = calculate_magnitude(snailfish_num)
    % Calculates the magnitude of a snailfish number recursively.
    if isnumeric(snailfish_num)
        magnitude = snailfish_num;
    elseif iscell(snailfish_num) && length(snailfish_num) == 2
        magnitude = 3 * calculate_magnitude(snailfish_num{1}) + 2 * calculate_magnitude(snailfish_num{2});
    else
        error('Invalid snailfish number format for magnitude calculation.');
    end
end

% Call the main function to start the program
main();
