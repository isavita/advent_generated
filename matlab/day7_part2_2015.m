
function solve_circuit
    rules = read_rules('input.txt');
    wire_map = containers.Map('KeyType', 'char', 'ValueType', 'char');
    for i = 1:length(rules)
        wire_map(rules(i).wire) = rules(i).rule;
    end

    memo = containers.Map('KeyType', 'char', 'ValueType', 'uint16');
    a_signal = evaluate_wire('a', wire_map, memo);

    if isKey(wire_map, 'b')
        wire_map('b') = num2str(a_signal);
    end

    memo = containers.Map('KeyType', 'char', 'ValueType', 'uint16');
    fprintf('%u\n', evaluate_wire('a', wire_map, memo));
end

function rules = read_rules(filename)
    fid = fopen(filename, 'r');
    if fid == -1
        error('Error opening file: %s', filename);
    end
    
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    lines = lines{1};
    fclose(fid);

    rules = struct('wire', {}, 'rule', {});
    for i = 1:length(lines)
        parts = strsplit(lines{i}, ' -> ');
        wire_name = parts{2};
        rule_str = parts{1};
        
        rules(end+1).wire = wire_name;
        rules(end).rule = rule_str;
    end
end

function value = evaluate_wire(wire_name, wire_map, memo)
    if isKey(memo, wire_name)
        value = memo(wire_name);
        return;
    end

    if ~isKey(wire_map, wire_name)
        if isstrprop(wire_name, 'digit')
            value = uint16(str2double(wire_name));
            return;
        else
            error('Wire %s not found in rules.', wire_name);
        end
    end

    rule = wire_map(wire_name);
    parts = strsplit(rule, ' ');

    if length(parts) == 1
        value = evaluate_wire(parts{1}, wire_map, memo);
    elseif length(parts) == 2 && strcmp(parts{1}, 'NOT')
        value = bitcmp(evaluate_wire(parts{2}, wire_map, memo));
    elseif length(parts) == 3
        operand1 = evaluate_wire(parts{1}, wire_map, memo);
        operator = parts{2};
        operand2 = evaluate_wire(parts{3}, wire_map, memo);

        switch operator
            case 'AND'
                value = bitand(operand1, operand2);
            case 'OR'
                value = bitor(operand1, operand2);
            case 'LSHIFT'
                value = bitshift(operand1, double(operand2));
            case 'RSHIFT'
                value = bitshift(operand1, -double(operand2));
            otherwise
                error('Unknown operator: %s', operator);
        end
    else
        error('Invalid rule format: %s', rule);
    end

    memo(wire_name) = value;
end
