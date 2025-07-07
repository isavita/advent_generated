
function main()
    % Read input from input.txt
    try
        file_id = fopen('input.txt', 'r');
        if file_id == -1
            error('Could not open input.txt for reading.');
        end
        lines = textscan(file_id, '%s', 'Delimiter', '\n');
        fclose(file_id);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % Parse the monkey jobs
    monkey_jobs = containers.Map();
    for i = 1:length(lines{1})
        line = lines{1}{i};
        parts = strsplit(line, ': ');
        monkey_name = parts{1};
        job_description = parts{2};
        monkey_jobs(monkey_name) = job_description;
    end

    % Solve the riddle using recursion with memoization
    memo = containers.Map();
    result = solve_monkey_riddle('root', monkey_jobs, memo);

    % Print the output to standard output
    fprintf('The monkey named root will yell: %d\n', result);
end

function result = solve_monkey_riddle(monkey_name, monkey_jobs, memo)
    % Check if the result is already memoized
    if isKey(memo, monkey_name)
        result = memo(monkey_name);
        return;
    end

    job_description = monkey_jobs(monkey_name);
    parts = strsplit(job_description, ' ');

    if length(parts) == 1
        % It's a number-yelling monkey
        result = str2double(job_description);
    else
        % It's a math operation monkey
        operand1_name = parts{1};
        operator = parts{2};
        operand2_name = parts{3};

        operand1_value = solve_monkey_riddle(operand1_name, monkey_jobs, memo);
        operand2_value = solve_monkey_riddle(operand2_name, monkey_jobs, memo);

        switch operator
            case '+'
                result = operand1_value + operand2_value;
            case '-'
                result = operand1_value - operand2_value;
            case '*'
                result = operand1_value * operand2_value;
            case '/'
                % Integer division as per the problem description's implication
                result = floor(operand1_value / operand2_value);
            otherwise
                error('Unknown operator: %s', operator);
        end
    end

    % Memoize the result before returning
    memo(monkey_name) = result;
end

% Call the main function to start the program
main();
