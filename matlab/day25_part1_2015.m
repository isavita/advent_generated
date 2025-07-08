
function main()
    % Read input from file
    data = fileread('input.txt');

    % Use regular expression to extract the row and column from the input.
    tokens = regexp(data, 'row (\d+), column (\d+)', 'tokens', 'once');

    if isempty(tokens)
        error('Invalid input format.');
    end

    row = str2double(tokens{1});
    column = str2double(tokens{2});

    % Calculate the position in the code sequence for the given row and column
    pos = get_position(row, column);

    % Calculate the code at the specified position
    code = get_code(pos);

    fprintf('%d\n', code);
end

function p = get_position(row, column)
    s = row + column - 2;
    p = s * (s + 1) / 2 + column;
end

function c = get_code(position)
    start_code = 20151125;
    multiplier = 252533;
    modulus = 33554393;

    if position == 1
        c = start_code;
        return;
    end

    exponent = position - 1;
    
    exp_mod_val = 1;
    base = multiplier;
    base = mod(base, modulus); 
    
    while exponent > 0
        if mod(exponent, 2) == 1
            exp_mod_val = mod(exp_mod_val * base, modulus);
        end
        base = mod(base * base, modulus);
        exponent = floor(exponent / 2);
    end
    
    c = mod(start_code * exp_mod_val, modulus);
end

% Call the main function to execute the script
main();
