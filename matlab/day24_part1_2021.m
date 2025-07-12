
function main()
    % Initialize arrays for parsed values (1-indexed for MATLAB)
    k = zeros(1, 14);
    l = zeros(1, 14);
    m = zeros(1, 14);

    % Open the input file
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file input.txt');
    end

    i = 0; % Line counter (0-indexed, matching C logic)
    while ~feof(fid)
        line = fgetl(fid);
        if ~ischar(line), break, end % End of file or error

        mod_val = mod(i, 18);
        block_idx = floor(i / 18) + 1; % Convert to 1-indexed block for MATLAB arrays

        % Parse specific lines based on their position within each 18-line block
        switch mod_val
            case 4 % Line "div z %d"
                val = sscanf(line, 'div z %d');
                l(block_idx) = val;
            case 5 % Line "add x %d"
                val = sscanf(line, 'add x %d');
                k(block_idx) = val;
            case 15 % Line "add y %d"
                val = sscanf(line, 'add y %d');
                m(block_idx) = val;
        end
        i = i + 1;
    end
    fclose(fid);

    % Calculate constraints based on the ALU program structure
    constraints = zeros(14, 2); % Stores [paired_index, offset_value]
    stack = zeros(1, 14);       % Simulates a stack for indices
    stackSize = 0;              % Current size of the stack

    for i = 1:14 % Iterate through each of the 14 blocks (1-indexed)
        if l(i) == 1
            % If l(i) is 1, it corresponds to a 'push' operation
            stackSize = stackSize + 1;
            stack(stackSize) = i; % Push current block index onto stack
        elseif l(i) == 26
            % If l(i) is 26, it corresponds to a 'pop' operation
            pop_idx = stack(stackSize); % Get the index from the top of the stack
            stackSize = stackSize - 1;  % Pop the element

            % Establish a constraint between the popped index and the current index
            % constraints(pop_idx, 1) stores the current index (j)
            % constraints(pop_idx, 2) stores the offset (m[pop_idx] + k[current_idx])
            constraints(pop_idx, 1) = i;
            constraints(pop_idx, 2) = m(pop_idx) + k(i);
        end
    end

    % Determine the digits for the largest valid model number
    max_val = zeros(1, 14); % Stores the 14 digits of the result

    for i = 1:14 % Iterate through each block index (1-indexed)
        % Skip if this index is not the 'push' part of a constraint pair
        if constraints(i, 1) == 0 && constraints(i, 2) == 0
            continue;
        end

        % Find the largest possible digit for max_val(i) (w_i)
        % such that 1 <= w_i <= 9 and 1 <= w_i + constraints(i,2) <= 9
        % This is equivalent to w_i = min(9, 9 - constraints(i,2))
        % The problem guarantees a solution exists, so w_i will be >= 1.
        vmax = 9;
        while (vmax + constraints(i,2) > 9)
            vmax = vmax - 1;
        end
        
        % Assign the calculated digit to max_val(i)
        max_val(i) = vmax;
        
        % Assign the corresponding digit to the paired index
        % max_val(j) = w_j = w_i + offset
        max_val(constraints(i,1)) = vmax + constraints(i,2);
    end

    % Construct the final 14-digit number from the max_val array
    n = 0;
    for i = 1:14
        n = n * 10 + max_val(i);
    end

    % Print the result. Use '%.0f' to print as a whole number without scientific notation.
    % MATLAB's default double precision can accurately represent 14-digit integers.
    fprintf('%.0f\n', n);
end
