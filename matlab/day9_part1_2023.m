
function main()
    % Read input from input.txt
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        lines = textscan(fid, '%s', 'Delimiter', '\n');
        fclose(fid);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    total_prediction = 0;

    % Process each history
    for i = 1:length(lines{1})
        history_str = lines{1}{i};
        history = sscanf(history_str, '%d');

        % Calculate the prediction for the current history
        prediction = extrapolate_next_value(history);
        total_prediction = total_prediction + prediction;
    end

    % Print the total sum to standard output
    fprintf('%d\n', total_prediction);
end

function next_value = extrapolate_next_value(history)
    % This function takes a history (a vector of numbers) and extrapolates
    % the next value.

    % Store all sequences of differences. The last element of each sequence
    % will be used to extrapolate upwards.
    sequences = {history};

    % Generate sequences of differences until an all-zero sequence is found
    while ~all(sequences{end} == 0)
        current_sequence = sequences{end};
        next_sequence = diff(current_sequence);
        sequences{end+1} = next_sequence;
    end

    % Extrapolate the next value by working upwards from the all-zero sequence
    % Start by adding a zero to the end of the last sequence (which is all zeros)
    sequences{end}(end+1) = 0;

    % Iterate from the second-to-last sequence up to the first sequence
    for i = length(sequences) - 1:-1:1
        % The value to add is the last element of the sequence below it.
        value_to_add = sequences{i+1}(end);
        % The new next value is the last element of the current sequence plus
        % the value_to_add.
        new_next_value = sequences{i}(end) + value_to_add;
        % Append this new value to the current sequence.
        sequences{i}(end+1) = new_next_value;
    end

    % The extrapolated next value for the original history is the last element
    % of the first sequence.
    next_value = sequences{1}(end);
end

% Call the main function to start the program
main();
