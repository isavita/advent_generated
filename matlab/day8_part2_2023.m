
function main()
    filename = 'input.txt';
    [instructions, node_map, node_left_indices, node_right_indices, node_names] = parse_input(filename);
    result = solve(instructions, node_map, node_left_indices, node_right_indices, node_names);
    fprintf('%d\n', result);
end

function [instructions, node_map, node_left_indices, node_right_indices, node_names] = parse_input(filename)
    fid = fopen(filename, 'r');
    instructions = fgetl(fid);
    fgetl(fid);

    node_map = containers.Map;
    node_left_names_temp = {};
    node_right_names_temp = {};
    node_names = {};
    node_count = 0;

    while ~feof(fid)
        line = fgetl(fid);
        if ~ischar(line) || isempty(line)
            continue;
        end
        tokens = regexp(line, '(\w{3}) = \((\w{3}), (\w{3})\)', 'tokens');
        if ~isempty(tokens)
            node_count = node_count + 1;
            name = tokens{1}{1};
            left_name = tokens{1}{2};
            right_name = tokens{1}{3};

            node_map(name) = node_count;
            node_names{node_count} = name;
            node_left_names_temp{node_count} = left_name;
            node_right_names_temp{node_count} = right_name;
        end
    end
    fclose(fid);

    num_nodes = node_count;
    node_left_indices = zeros(num_nodes, 1);
    node_right_indices = zeros(num_nodes, 1);

    for i = 1:num_nodes
        node_left_indices(i) = node_map(node_left_names_temp{i});
        node_right_indices(i) = node_map(node_right_names_temp{i});
    end
end

function result = solve(instructions, node_map, node_left_indices, node_right_indices, node_names)
    start_node_indices = [];
    for i = 1:length(node_names)
        if endsWith(node_names{i}, 'A')
            start_node_indices = [start_node_indices, i];
        end
    end

    if isempty(start_node_indices)
        result = int64(0);
        return;
    end

    steps_per_path = zeros(1, length(start_node_indices), 'int64');
    instructions_length = length(instructions);

    for i = 1:length(start_node_indices)
        current_node_idx = start_node_indices(i);
        steps = int64(0);
        
        while ~endsWith(node_names{current_node_idx}, 'Z')
            instruction_char = instructions(mod(steps, instructions_length) + 1);
            
            if instruction_char == 'L'
                current_node_idx = node_left_indices(current_node_idx);
            else
                current_node_idx = node_right_indices(current_node_idx);
            end
            steps = steps + 1;
        end
        steps_per_path(i) = steps;
    end

    result = steps_per_path(1);
    for i = 2:length(steps_per_path)
        result = lcm(result, steps_per_path(i));
    end
end
