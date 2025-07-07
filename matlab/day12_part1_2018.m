
function main()
    % Read input from input.txt
    rules = read_rules('input.txt');
    initial_state_str = read_initial_state('input.txt');

    % Convert initial state string to a logical array
    initial_state = initial_state_str == '#';

    % Define the number of generations
    num_generations = 20;

    % Simulate generations
    final_state = simulate_generations(initial_state, rules, num_generations);

    % Calculate the sum of plant pot indices
    sum_of_plants = calculate_sum_of_plants(final_state);

    % Print the result to standard output
    fprintf('Sum of plant pot indices after %d generations: %d\n', num_generations, sum_of_plants);
end

function rules = read_rules(filename)
    % Reads the plant spreading rules from the input file.
    % Returns a map where keys are the 5-char patterns and values are the resulting plant state.
    rules = containers.Map();
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end

    % Skip the initial state line
    fgetl(fid);

    % Read the rules
    line = fgetl(fid);
    while ischar(line)
        if ~isempty(line)
            pattern = line(1:5);
            result = line(10);
            rules(pattern) = result;
        end
        line = fgetl(fid);
    end
    fclose(fid);
end

function initial_state_str = read_initial_state(filename)
    % Reads the initial state string from the input file.
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end
    initial_state_str = strsplit(fgetl(fid), ': ');
    initial_state_str = initial_state_str{2};
    fclose(fid);
end

function final_state = simulate_generations(initial_state, rules, num_generations)
    % Simulates the plant growth for a given number of generations.
    % initial_state: a logical array representing the current state of pots.
    % rules: a containers.Map of spreading rules.
    % num_generations: the number of generations to simulate.

    current_state = initial_state;
    offset = 0; % To keep track of the index of pot 0

    for gen = 1:num_generations
        % Determine the bounds of the current state to consider
        first_plant = find(current_state, 1, 'first');
        last_plant = find(current_state, 1, 'last');

        % Extend the state to ensure we have enough context for all rules
        % We need at least 2 pots to the left and 2 to the right of the current plants.
        % The effective range of pots to consider for the next generation is
        % from first_plant - 2 to last_plant + 2.
        min_idx = max(1, first_plant - 2);
        max_idx = min(length(current_state), last_plant + 2);

        % If the current state is empty, we might need to expand it
        if isempty(first_plant)
            min_idx = 1;
            max_idx = 1;
        end

        % Pad the state with empty pots if necessary
        padding_left = max(0, 2 - min_idx + 1);
        padding_right = max(0, (max_idx + 2) - length(current_state));

        extended_state = [false(1, padding_left), current_state, false(1, padding_right)];
        offset = offset - padding_left; % Adjust offset for left padding

        next_state = false(size(extended_state));

        % Iterate through each pot to determine its state in the next generation
        for i = 1:length(extended_state)
            % Define the 5-pot pattern around the current pot
            % Ensure we don't go out of bounds by using logical indexing and padding
            pattern_indices = max(1, i-2):min(length(extended_state), i+2);
            
            % Create the 5-character pattern string
            current_pattern_chars = repmat('.', 1, 5);
            
            % Fill the pattern based on the extended_state
            pattern_start_idx = 1;
            for k = pattern_indices
                if k >= 1 && k <= length(extended_state)
                    if extended_state(k)
                        current_pattern_chars(pattern_start_idx) = '#';
                    end
                end
                pattern_start_idx = pattern_start_idx + 1;
            end
            
            % Ensure the pattern is exactly 5 characters, padding with '.' if needed
            if length(current_pattern_chars) < 5
                current_pattern_chars = [current_pattern_chars, repmat('.', 1, 5 - length(current_pattern_chars))];
            end
            
            % Look up the rule for this pattern
            if isKey(rules, current_pattern_chars)
                if rules(current_pattern_chars) == '#'
                    next_state(i) = true;
                end
            end
        end

        % Trim leading and trailing empty pots to keep the state compact
        first_next_plant = find(next_state, 1, 'first');
        last_next_plant = find(next_state, 1, 'last');

        if isempty(first_next_plant)
            current_state = false(1); % If no plants, state becomes empty
            offset = 0;
        else
            current_state = next_state(first_next_plant:last_next_plant);
            offset = offset + first_next_plant - 1; % Adjust offset based on trimmed start
        end
        
        % Optional: Print state for debugging
        % fprintf('Gen %d (offset %d): %s\n', gen, offset, char(current_state + '0'));
    end
    
    % Return the final state and its offset
    final_state.pots = current_state;
    final_state.offset = offset;
end

function sum_of_plants = calculate_sum_of_plants(state_info)
    % Calculates the sum of the indices of pots containing plants.
    % state_info: a struct containing 'pots' (logical array) and 'offset' (integer).
    
    sum_of_plants = 0;
    for i = 1:length(state_info.pots)
        if state_info.pots(i)
            % The index of the pot is its position in the array plus the offset
            pot_index = (i - 1) + state_info.offset;
            sum_of_plants = sum_of_plants + pot_index;
        end
    end
end

% Call the main function to execute the program
main();
