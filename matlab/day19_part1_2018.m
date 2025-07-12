
function main()
    % Open the input file
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file input.txt');
    end

    % Read the IP binding line
    ipLine = fgetl(fid);
    ipBind = sscanf(ipLine, '#ip %d');

    % Read all instructions
    % textscan returns a cell array for strings and numeric arrays for numbers
    data = textscan(fid, '%s %d %d %d');
    fclose(fid);

    opcodes = data{1};
    operandsA = data{2};
    operandsB = data{3};
    operandsC = data{4};
    
    numInstructions = length(opcodes);

    % Initialize registers (MATLAB is 1-indexed, so registers 1-6 correspond to R0-R5)
    registers = zeros(1, 6); 

    % Current instruction pointer (0-indexed, like in C)
    ip = 0; 

    % Simulation loop
    while ip >= 0 && ip < numInstructions
        % Store the current instruction pointer value into the bound register
        registers(ipBind + 1) = ip; 

        % Get the current instruction's details
        currentOpcode = opcodes{ip + 1};
        a = operandsA(ip + 1);
        b = operandsB(ip + 1);
        c = operandsC(ip + 1);

        % Execute the instruction based on the opcode
        switch currentOpcode
            case 'addr'
                registers(c + 1) = registers(a + 1) + registers(b + 1);
            case 'addi'
                registers(c + 1) = registers(a + 1) + b;
            case 'mulr'
                registers(c + 1) = registers(a + 1) * registers(b + 1);
            case 'muli'
                registers(c + 1) = registers(a + 1) * b;
            case 'banr'
                registers(c + 1) = bitand(registers(a + 1), registers(b + 1));
            case 'bani'
                registers(c + 1) = bitand(registers(a + 1), b);
            case 'borr'
                registers(c + 1) = bitor(registers(a + 1), registers(b + 1));
            case 'bori'
                registers(c + 1) = bitor(registers(a + 1), b);
            case 'setr'
                registers(c + 1) = registers(a + 1);
            case 'seti'
                registers(c + 1) = a;
            case 'gtir'
                registers(c + 1) = (a > registers(b + 1)); % MATLAB logicals (true/false) convert to 1/0
            case 'gtri'
                registers(c + 1) = (registers(a + 1) > b);
            case 'gtrr'
                registers(c + 1) = (registers(a + 1) > registers(b + 1));
            case 'eqir'
                registers(c + 1) = (a == registers(b + 1));
            case 'eqri'
                registers(c + 1) = (registers(a + 1) == b);
            case 'eqrr'
                registers(c + 1) = (registers(a + 1) == registers(b + 1));
            otherwise
                error('Unknown opcode: %s', currentOpcode);
        end

        % Read the next instruction pointer value from the bound register
        ip = registers(ipBind + 1);
        
        % Increment the instruction pointer for the next cycle,
        % mimicking the C for loop's implicit increment.
        ip = ip + 1;
    end

    % Print the value of register 0 (which is registers(1) in MATLAB)
    fprintf('%d\n', registers(1));
end
