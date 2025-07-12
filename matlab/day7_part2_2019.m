
function main()
    % Read input from "input.txt"
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end
    line = fgetl(fid);
    fclose(fid);

    % Parse the comma-separated integers
    initial_code_str = strsplit(line, ',');
    initial_code = str2double(initial_code_str);

    % Intcode memory can grow. Pre-allocate to a reasonable maximum size
    % as per C code's MAX_CODE_SIZE, filling with zeros.
    MAX_CODE_SIZE = 4096;
    if length(initial_code) < MAX_CODE_SIZE
        initial_code(MAX_CODE_SIZE) = 0; % Pad with zeros
    end

    % Phase settings for feedback loop
    phase_settings_base = [5, 6, 7, 8, 9];
    all_permutations = perms(phase_settings_base); % Generate all permutations

    max_output = -inf; % Initialize with negative infinity for finding maximum

    num_perms = size(all_permutations, 1);
    for i = 1:num_perms
        current_phase_settings = all_permutations(i, :);
        current_output = run_feedback_loop(current_phase_settings, initial_code);
        if current_output > max_output
            max_output = current_output;
        end
    end

    fprintf('%d\n', max_output);
end

% Helper function to simulate the feedback loop of amplifiers
function final_output = run_feedback_loop(phase_settings, initial_code)
    num_amplifiers = length(phase_settings);

    % Initialize state for each amplifier
    vm_codes = cell(1, num_amplifiers); % Each VM gets its own copy of the code
    vm_ips = ones(1, num_amplifiers); % Instruction Pointers (1-indexed)
    vm_halted = false(1, num_amplifiers); % Halt status
    vm_needs_input = false(1, num_amplifiers); % Flag if VM is waiting for input
    vm_first_input_given = false(1, num_amplifiers); % Flag for phase setting input

    for i = 1:num_amplifiers
        vm_codes{i} = initial_code; % Deep copy of initial code for each VM
    end

    current_amp_idx = 1; % Start with amplifier A (index 1)
    signal = 0; % Initial input signal to amplifier A

    % Main feedback loop simulation
    while any(~vm_halted) % Continue as long as at least one VM is not halted
        % Get current VM's state
        current_code = vm_codes{current_amp_idx};
        current_ip = vm_ips(current_amp_idx);

        % Determine input for the current VM
        vm_input = []; % Default to no input
        if ~vm_first_input_given(current_amp_idx)
            % First input is the phase setting
            vm_input = phase_settings(current_amp_idx);
            vm_first_input_given(current_amp_idx) = true;
        elseif vm_needs_input(current_amp_idx)
            % Subsequent inputs are signals from previous amplifier
            vm_input = signal;
            vm_needs_input(current_amp_idx) = false; % Reset flag
        end

        % Run the VM until it halts, needs input, or produces output
        [new_code, new_ip, output_val, halted, needs_input] = run_vm(current_code, current_ip, vm_input);

        % Update the VM's state
        vm_codes{current_amp_idx} = new_code;
        vm_ips(current_amp_idx) = new_ip;
        vm_halted(current_amp_idx) = halted;
        vm_needs_input(current_amp_idx) = needs_input;

        if ~isempty(output_val) % If the VM produced an output
            signal = output_val; % This output becomes the input for the next amplifier
        end

        % Move to the next amplifier in the loop
        current_amp_idx = mod(current_amp_idx, num_amplifiers) + 1;

        % Optimization: If all VMs are halted, we can stop early
        if all(vm_halted)
            break;
        end
    end

    final_output = signal; % The last signal produced by the loop is the result
end

% Intcode Virtual Machine implementation
function [code, ip, output_val, halted, needs_input] = run_vm(code, ip, input_val)
    output_val = []; % Output value, empty if no output produced
    halted = false; % VM halted status
    needs_input = false; % VM waiting for input status

    % Nested helper function to get parameter value based on mode
    function val = get_param(offset, mode)
        param_val_idx = ip + offset;
        % Check if the instruction parameter itself is out of bounds
        if param_val_idx > length(code)
            error('Program error: Attempted to read instruction parameter address out of bounds at IP %d, offset %d', ip, offset);
        end
        param_val = code(param_val_idx); % This is the immediate value or an address

        if mode == 0 % Position mode
            % Access value at the address specified by param_val
            % If address is beyond current code size, value is implicitly 0
            if param_val + 1 > length(code)
                val = 0;
            else
                val = code(param_val + 1);
            end
        else % Immediate mode
            val = param_val;
        end
    end

    % Nested helper function to set value (always position mode for writes)
    function set_val(offset, value)
        addr_idx = ip + offset;
        % Check if the instruction parameter itself is out of bounds
        if addr_idx > length(code)
            error('Program error: Attempted to read write address out of bounds at IP %d, offset %d', ip, offset);
        end
        addr = code(addr_idx) + 1; % This is the address to write to (MATLAB 1-indexed)

        % Dynamically expand memory if needed for write, filling with zeros
        if addr > length(code)
            code(addr) = 0; % MATLAB automatically pads with zeros
        end
        code(addr) = value;
    end

    % Main VM execution loop
    while ip <= length(code)
        instruction = code(ip);
        opcode = mod(instruction, 100);
        mode1 = floor(instruction / 100) - floor(instruction / 1000) * 10;
        mode2 = floor(instruction / 1000) - floor(instruction / 10000) * 10;
        % Mode 3 (for write addresses) is always position mode, so no need for mode3

        switch opcode
            case 1 % Add
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                set_val(3, p1 + p2);
                ip = ip + 4;
            case 2 % Multiply
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                set_val(3, p1 * p2);
                ip = ip + 4;
            case 3 % Input
                if isempty(input_val)
                    needs_input = true;
                    return; % Pause execution, waiting for input
                end
                set_val(1, input_val);
                input_val = []; % Consume the input
                ip = ip + 2;
            case 4 % Output
                p1 = get_param(1, mode1);
                output_val = p1;
                ip = ip + 2;
                return; % Output produced, pause execution
            case 5 % Jump-if-true
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                if p1 ~= 0
                    ip = p2 + 1; % Jump to new IP (MATLAB 1-indexed)
                else
                    ip = ip + 3;
                end
            case 6 % Jump-if-false
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                if p1 == 0
                    ip = p2 + 1; % Jump to new IP (MATLAB 1-indexed)
                else
                    ip = ip + 3;
                end
            case 7 % Less than
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                set_val(3, (p1 < p2)); % MATLAB boolean (true=1, false=0)
                ip = ip + 4;
            case 8 % Equals
                p1 = get_param(1, mode1);
                p2 = get_param(2, mode2);
                set_val(3, (p1 == p2)); % MATLAB boolean (true=1, false=0)
                ip = ip + 4;
            case 99 % Halt
                halted = true;
                return;
            otherwise
                error('Unknown opcode: %d at ip %d', opcode, ip);
        end
    end
end

% Call the main function to execute the program
main();
