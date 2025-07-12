
function main()
% main is the entry point for the program.
% It reads gate definitions from "input.txt", simulates a full adder,
% identifies and corrects swapped gate outputs, then prints the corrected pairs.

    % Define constants
    NUM_PAIRS = 4;

    % Persistent variable to mimic global 'gates' array from C
    persistent gates;
    gates = struct('a', {}, 'op', {}, 'b', {}, 'output', {}); % Initialize empty struct array

    % Read input file
    try
        fileContent = fileread('input.txt');
    catch ME
        fprintf(2, 'Error reading input.txt: %s\n', ME.message);
        return;
    end

    % Parse gate definitions
    definitionsStart = strfind(fileContent, sprintf('\n\n'));
    if isempty(definitionsStart)
        definitions = fileContent;
    else
        definitions = fileContent(definitionsStart(1)+2:end);
    end

    lines = strsplit(definitions, '\n');
    lines = lines(~cellfun('isempty', lines)); % Remove empty lines

    gate_count = 0;
    numZ = 0;

    for i = 1:length(lines)
        line = strtrim(lines{i});
        if isempty(line)
            continue;
        end

        % Parse line: "a op b -> output"
        parts = strsplit(line, ' -> ');
        if length(parts) == 2
            input_part = parts{1};
            output_name = parts{2};

            input_comps = strsplit(input_part, ' ');
            if length(input_comps) == 3
                gate_count = gate_count + 1;
                gates(gate_count).a = input_comps{1};
                gates(gate_count).op = input_comps{2};
                gates(gate_count).b = input_comps{3};
                gates(gate_count).output = output_name;

                if ~isempty(output_name) && output_name(1) == 'z'
                    numZ = numZ + 1;
                end
            end
        end
    end

    % Helper functions (nested functions can access 'gates' directly)

    function output_name = find_output_by_gate(a1, op, b1)
        % Finds the output name of a gate given its inputs and operation.
        % Handles commutative operations (A op B == B op A).
        output_name = '';
        for k = 1:gate_count
            g = gates(k);
            if strcmp(g.op, op)
                if (strcmp(g.a, a1) && strcmp(g.b, b1)) || ...
                   (strcmp(g.a, b1) && strcmp(g.b, a1))
                    output_name = g.output;
                    return;
                end
            end
        end
    end

    function gate_struct = find_gate_by_output(output_name)
        % Finds a gate struct given its output name.
        gate_struct = struct('a', '', 'op', '', 'b', '', 'output', ''); % Initialize empty
        for k = 1:gate_count
            if strcmp(gates(k).output, output_name)
                gate_struct = gates(k);
                return;
            end
        end
    end

    function swap_gate_outputs_internal(out1, out2)
        % Swaps the output names of gates matching out1 or out2.
        % Modifies the persistent 'gates' variable.
        for k = 1:gate_count
            if strcmp(gates(k).output, out1)
                gates(k).output = out2;
            elseif strcmp(gates(k).output, out2)
                gates(k).output = out1;
            end
        end
    end

    % Main logic for full adder simulation and correction
    swapped_pairs = cell(1, NUM_PAIRS * 2);
    pair_count = 0;

    while pair_count < NUM_PAIRS
        carry = ''; % Empty string for no carry
        for i = 0:numZ-1
            xi = sprintf('x%02d', i);
            yi = sprintf('y%02d', i);
            zi = sprintf('z%02d', i);

            adder = '';
            next_carry = '';

            if i == 0 % First bit (half adder)
                adder = find_output_by_gate(xi, 'XOR', yi);
                next_carry = find_output_by_gate(xi, 'AND', yi);
            else % Subsequent bits (full adder)
                bit = find_output_by_gate(xi, 'XOR', yi);
                if ~isempty(bit) && ~isempty(carry)
                    adder = find_output_by_gate(bit, 'XOR', carry);
                    if ~isempty(adder)
                        c1 = find_output_by_gate(xi, 'AND', yi);
                        c2 = find_output_by_gate(bit, 'AND', carry);
                        if ~isempty(c1) && ~isempty(c2)
                            next_carry = find_output_by_gate(c1, 'OR', c2);
                        end
                    end
                end
            end

            swapped = false;
            if ~isempty(adder) && ~strcmp(adder, zi)
                % Case 1: Adder output found but is not the expected 'zi'
                swapped_pairs{pair_count * 2 + 1} = adder;
                swapped_pairs{pair_count * 2 + 2} = zi;
                swap_gate_outputs_internal(adder, zi);
                swapped = true;
            elseif isempty(adder)
                % Case 2: Adder output not found, implies an input to 'zi' gate is wrong
                gate_z = find_gate_by_output(zi);
                bit = find_output_by_gate(xi, 'XOR', yi); % Recalculate bit if needed

                % Check if gate_z exists, and bit and carry are available
                if ~isempty(fieldnames(gate_z)) && ~isempty(bit) && ~isempty(carry)
                    % C logic: Check if gate_z.a is an input to an XOR gate with 'carry'
                    if ~isempty(find_output_by_gate(gate_z.a, 'XOR', carry))
                        swapped_pairs{pair_count * 2 + 1} = bit;
                        swapped_pairs{pair_count * 2 + 2} = gate_z.a;
                        swap_gate_outputs_internal(bit, gate_z.a);
                        swapped = true;
                    % C logic: Check if gate_z.b is an input to an XOR gate with 'carry'
                    elseif ~isempty(find_output_by_gate(gate_z.b, 'XOR', carry))
                        swapped_pairs{pair_count * 2 + 1} = bit;
                        swapped_pairs{pair_count * 2 + 2} = gate_z.b;
                        swap_gate_outputs_internal(bit, gate_z.b);
                        swapped = true;
                    end
                end
            end

            if swapped
                pair_count = pair_count + 1;
                break; % Break inner loop and try to find next pair
            end
            carry = next_carry; % Update carry for next iteration
        end
    end

    % Remove unused pre-allocated cells if NUM_PAIRS was not reached
    swapped_pairs = swapped_pairs(1:pair_count * 2);

    % Sort the swapped pairs alphabetically
    swapped_pairs = sort(swapped_pairs);

    % Print the result as a comma-separated string
    fprintf('%s\n', strjoin(swapped_pairs, ','));

end
