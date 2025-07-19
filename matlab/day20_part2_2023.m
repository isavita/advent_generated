
function main()
    BROADCASTER = 1;
    FLIP_FLOP = 2;
    CONJUNCTION = 3;

    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    lines = lines{1};
    fclose(fid);

    num_modules = numel(lines);
    name_to_idx = containers.Map('KeyType', 'char', 'ValueType', 'double');
    temp_modules = cell(num_modules, 1);

    for i = 1:num_modules
        parts = strsplit(lines{i}, ' -> ');
        name_part = parts{1};
        name = regexprep(name_part, '[%&]', '');
        name_to_idx(name) = i;
        temp_modules{i} = struct('name_part', name_part, 'outputs_str', parts{2});
    end

    modules = struct('type', 0, 'outputs', [], 'state', false, 'inputs', [], 'memory', [], 'output_names', {{}});
    modules = repmat(modules, num_modules, 1);

    for i = 1:num_modules
        name_part = temp_modules{i}.name_part;
        if strcmp(name_part, 'broadcaster')
            modules(i).type = BROADCASTER;
        elseif startsWith(name_part, '%')
            modules(i).type = FLIP_FLOP;
        else
            modules(i).type = CONJUNCTION;
        end

        output_names = strsplit(temp_modules{i}.outputs_str, ', ');
        modules(i).output_names = output_names;
        out_indices = [];
        for j = 1:numel(output_names)
            if isKey(name_to_idx, output_names{j})
                out_indices(end+1) = name_to_idx(output_names{j});
            else
                out_indices(end+1) = 0;
            end
        end
        modules(i).outputs = out_indices;
    end

    for i = 1:num_modules
        for j = 1:numel(modules(i).outputs)
            target_idx = modules(i).outputs(j);
            if target_idx > 0 && modules(target_idx).type == CONJUNCTION
                modules(target_idx).inputs(end+1) = i;
            end
        end
    end

    for i = 1:num_modules
        if modules(i).type == CONJUNCTION
            modules(i).memory = false(1, numel(modules(i).inputs));
        end
    end

    broadcaster_idx = name_to_idx('broadcaster');
    feeder_idx = 0;
    for i = 1:num_modules
        if any(strcmp(modules(i).output_names, 'rx'))
            feeder_idx = i;
            break;
        end
    end

    if feeder_idx == 0
        fprintf('0\n');
        return;
    end

    feeder_inputs = modules(feeder_idx).inputs;
    num_feeder_inputs = numel(feeder_inputs);
    loop_lengths = zeros(1, num_feeder_inputs, 'uint64');
    
    press_count = uint64(0);
    queue = zeros(16384, 3, 'double');

    while any(loop_lengths == 0)
        press_count = press_count + 1;
        
        head = 1;
        tail = 2;
        queue(1, :) = [broadcaster_idx, 0, 0];

        while head < tail
            current_pulse = queue(head, :);
            head = head + 1;
            to_idx = current_pulse(1);
            from_idx = current_pulse(2);
            pulse = current_pulse(3);

            if to_idx == feeder_idx && pulse == 1
                input_pos = find(feeder_inputs == from_idx, 1);
                if ~isempty(input_pos) && loop_lengths(input_pos) == 0
                    loop_lengths(input_pos) = press_count;
                end
            end

            if to_idx == 0, continue; end

            switch modules(to_idx).type
                case FLIP_FLOP
                    if pulse == 0
                        modules(to_idx).state = ~modules(to_idx).state;
                        pulse_to_send = modules(to_idx).state;
                        for k = 1:numel(modules(to_idx).outputs)
                            queue(tail, :) = [modules(to_idx).outputs(k), to_idx, pulse_to_send];
                            tail = tail + 1;
                        end
                    end
                case CONJUNCTION
                    input_pos = find(modules(to_idx).inputs == from_idx, 1);
                    modules(to_idx).memory(input_pos) = pulse;
                    pulse_to_send = double(~all(modules(to_idx).memory));
                    for k = 1:numel(modules(to_idx).outputs)
                        queue(tail, :) = [modules(to_idx).outputs(k), to_idx, pulse_to_send];
                        tail = tail + 1;
                    end
                otherwise % BROADCASTER
                    for k = 1:numel(modules(to_idx).outputs)
                        queue(tail, :) = [modules(to_idx).outputs(k), to_idx, pulse];
                        tail = tail + 1;
                    end
            end
        end
    end

    result = loop_lengths(1);
    for i = 2:numel(loop_lengths)
        result = lcm(result, loop_lengths(i));
    end
    
    fprintf('%lu\n', result);
end
