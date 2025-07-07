
function main()
    % Read input from input.txt
    samples = read_samples('input.txt');

    % Define all possible opcodes
    opcodes = define_opcodes();

    % Count samples that behave like three or more opcodes
    count = 0;
    for i = 1:length(samples)
        sample = samples{i};
        before = sample.before;
        instruction = sample.instruction;
        after = sample.after;

        possible_opcodes_count = 0;
        for j = 1:length(opcodes)
            opcode_func = opcodes{j}.func;
            opcode_name = opcodes{j}.name;
            
            % Simulate the opcode with the given instruction
            result_after = simulate_instruction(opcode_func, before, instruction);

            % Check if the simulated result matches the actual 'after' state
            if isequal(result_after, after)
                possible_opcodes_count = possible_opcodes_count + 1;
            end
        end

        if possible_opcodes_count >= 3
            count = count + 1;
        end
    end

    % Print the output to standard output
    fprintf('Number of samples behaving like three or more opcodes: %d\n', count);
end

function samples = read_samples(filename)
    samples = {};
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end

    while ~feof(fid)
        % Read 'Before' line
        before_line = fgetl(fid);
        if ~ischar(before_line)
            break; % End of file
        end
        if ~startsWith(before_line, 'Before:')
            continue; % Skip empty lines or other non-sample lines
        end
        before = sscanf(before_line, 'Before: [%d, %d, %d, %d]');

        % Read instruction line
        instruction_line = fgetl(fid);
        instruction = sscanf(instruction_line, '%d %d %d %d');

        % Read 'After' line
        after_line = fgetl(fid);
        after = sscanf(after_line, 'After:  [%d, %d, %d, %d]');

        % Read blank line between samples
        fgetl(fid);

        samples{end+1} = struct('before', before', 'instruction', instruction', 'after', after');
    end

    fclose(fid);
end

function opcodes = define_opcodes()
    opcodes = {
        struct('name', 'addr', 'func', @(r, a, b, c) addr(r, a, b, c)),
        struct('name', 'addi', 'func', @(r, a, b, c) addi(r, a, b, c)),
        struct('name', 'mulr', 'func', @(r, a, b, c) mulr(r, a, b, c)),
        struct('name', 'muli', 'func', @(r, a, b, c) muli(r, a, b, c)),
        struct('name', 'banr', 'func', @(r, a, b, c) banr(r, a, b, c)),
        struct('name', 'bani', 'func', @(r, a, b, c) bani(r, a, b, c)),
        struct('name', 'borr', 'func', @(r, a, b, c) borr(r, a, b, c)),
        struct('name', 'bori', 'func', @(r, a, b, c) bori(r, a, b, c)),
        struct('name', 'setr', 'func', @(r, a, b, c) setr(r, a, b, c)),
        struct('name', 'seti', 'func', @(r, a, b, c) seti(r, a, b, c)),
        struct('name', 'gtir', 'func', @(r, a, b, c) gtir(r, a, b, c)),
        struct('name', 'gtri', 'func', @(r, a, b, c) gtri(r, a, b, c)),
        struct('name', 'gtrr', 'func', @(r, a, b, c) gtrr(r, a, b, c)),
        struct('name', 'eqir', 'func', @(r, a, b, c) eqir(r, a, b, c)),
        struct('name', 'eqri', 'func', @(r, a, b, c) eqri(r, a, b, c)),
        struct('name', 'eqrr', 'func', @(r, a, b, c) eqrr(r, a, b, c))
    };
end

function result_registers = simulate_instruction(opcode_func, registers, instruction)
    % Create a copy of the registers to avoid modifying the original
    result_registers = registers;
    
    % Extract instruction parts
    opcode = instruction(1); % This is not used by the function pointer, but good for clarity
    a = instruction(2);
    b = instruction(3);
    c = instruction(4);

    % Execute the opcode function
    result_registers = opcode_func(result_registers, a, b, c);
end

% --- Opcode Implementations ---

% Addition
function r = addr(r, a, b, c)
    r(c+1) = r(a+1) + r(b+1);
end

function r = addi(r, a, b, c)
    r(c+1) = r(a+1) + b;
end

% Multiplication
function r = mulr(r, a, b, c)
    r(c+1) = r(a+1) * r(b+1);
end

function r = muli(r, a, b, c)
    r(c+1) = r(a+1) * b;
end

% Bitwise AND
function r = banr(r, a, b, c)
    r(c+1) = bitand(r(a+1), r(b+1));
end

function r = bani(r, a, b, c)
    r(c+1) = bitand(r(a+1), b);
end

% Bitwise OR
function r = borr(r, a, b, c)
    r(c+1) = bitor(r(a+1), r(b+1));
end

function r = bori(r, a, b, c)
    r(c+1) = bitor(r(a+1), b);
end

% Assignment
function r = setr(r, a, b, c)
    r(c+1) = r(a+1);
end

function r = seti(r, a, b, c)
    r(c+1) = a;
end

% Greater-than testing
function r = gtir(r, a, b, c)
    r(c+1) = (a > r(b+1));
end

function r = gtri(r, a, b, c)
    r(c+1) = (r(a+1) > b);
end

function r = gtrr(r, a, b, c)
    r(c+1) = (r(a+1) > r(b+1));
end

% Equality testing
function r = eqir(r, a, b, c)
    r(c+1) = (a == r(b+1));
end

function r = eqri(r, a, b, c)
    r(c+1) = (r(a+1) == b);
end

function r = eqrr(r, a, b, c)
    r(c+1) = (r(a+1) == r(b+1));
end

% --- Entry Point ---
% This ensures the script runs the main function when executed.
if nargin == 0
    main();
end
