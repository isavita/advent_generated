
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

    histories = cell(length(lines{1}), 1);
    for i = 1:length(lines{1})
        histories{i} = sscanf(lines{1}{i}, '%d');
    end

    % Calculate the sum of extrapolated next values (Part 1)
    totalNextValue = 0;
    for i = 1:length(histories)
        totalNextValue = totalNextValue + extrapolateNextValue(histories{i});
    end
    fprintf('Sum of extrapolated next values (Part 1): %d\n', totalNextValue);

    % Calculate the sum of extrapolated previous values (Part 2)
    totalPreviousValue = 0;
    for i = 1:length(histories)
        totalPreviousValue = totalPreviousValue + extrapolatePreviousValue(histories{i});
    end
    fprintf('Sum of extrapolated previous values (Part 2): %d\n', totalPreviousValue);
end

function nextVal = extrapolateNextValue(history)
    % Recursively calculates the next value in a history.
    % Base case: if all differences are zero, the next value is 0.
    if all(history == 0)
        nextVal = 0;
        return;
    end

    % Calculate the differences between consecutive elements.
    differences = diff(history);

    % Recursively find the next value of the differences.
    nextDiff = extrapolateNextValue(differences);

    % The next value of the current history is the last element plus the next value of the differences.
    nextVal = history(end) + nextDiff;
end

function prevVal = extrapolatePreviousValue(history)
    % Recursively calculates the previous value in a history.
    % Base case: if all differences are zero, the previous value is 0.
    if all(history == 0)
        prevVal = 0;
        return;
    end

    % Calculate the differences between consecutive elements.
    differences = diff(history);

    % Recursively find the previous value of the differences.
    prevDiff = extrapolatePreviousValue(differences);

    % The previous value of the current history is the first element minus the previous value of the differences.
    prevVal = history(1) - prevDiff;
end

% Call the main function to execute the program
main();
