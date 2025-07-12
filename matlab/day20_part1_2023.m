
function solve()
    file_id = fopen('input.txt', 'r');
    lines = textscan(file_id, '%s', 'Delimiter', '\n');
    fclose(file_id);
    lines = lines{1};

    modules = struct('name', {}, 'type', {}, 'destinations', {}, 'state', {}, 'memory', {});
    module_map = containers.Map('KeyType', 'char', 'ValueType', 'double');
    module_count = 0;

    for i = 1:length(lines)
        line = lines{i};
        parts = strsplit(line, ' -> ');
        source_part = parts{1};
        dest_part = parts{2};

        module_count = module_count + 1;
        current_module = struct();
        current_module.destinations = {};
        current_module.state = false;
        current_module.memory = containers.Map('KeyType', 'char', 'ValueType', 'logical');

        if startsWith(source_part, '%')
            current_module.type = 'flipflop';
            current_module.name = source_part(2:end);
        elseif startsWith(source_part, '&')
            current_module.type = 'conjunction';
            current_module.name = source_part(2:end);
        elseif strcmp(source_part, 'broadcaster')
            current_module.type = 'broadcaster';
            current_module.name = source_part;
        else
            current_module.type = 'other';
            current_module.name = source_part;
        end

        dest_names = strsplit(dest_part, ', ');
        current_module.destinations = dest_names;
        modules(module_count) = current_module;
        module_map(current_module.name) = module_count;
    end

    for i = 1:module_count
        for j = 1:length(modules(i).destinations)
            dest_name = modules(i).destinations{j};
            if isKey(module_map, dest_name)
                dest_idx = module_map(dest_name);
                if strcmp(modules(dest_idx).type, 'conjunction')
                    modules(dest_idx).memory(modules(i).name) = false;
                end
            end
        end
    end

    low_pulses = 0;
    high_pulses = 0;
    num_cycles = 1000;

    for cycle = 1:num_cycles
        queue = {};
        queue_head = 1;
        queue_tail = 1;

        broadcaster_idx = module_map('broadcaster');
        queue{queue_tail} = struct('value', 0, 'from', 'button', 'to', modules(broadcaster_idx).name);
        queue_tail = queue_tail + 1;

        while queue_head < queue_tail
            current_pulse = queue{queue_head};
            queue_head = queue_head + 1;

            if current_pulse.value == 0
                low_pulses = low_pulses + 1;
            else
                high_pulses = high_pulses + 1;
            end

            if ~isKey(module_map, current_pulse.to)
                continue;
            end

            to_idx = module_map(current_pulse.to);
            to_module = modules(to_idx);
            pulse_to_send = -1;
            send_pulse = true;

            switch to_module.type
                case 'flipflop'
                    if current_pulse.value == 0
                        to_module.state = ~to_module.state;
                        pulse_to_send = double(to_module.state);
                    else
                        send_pulse = false;
                    end
                case 'conjunction'
                    to_module.memory(current_pulse.from) = (current_pulse.value == 1);
                    all_high = true;
                    mem_keys = keys(to_module.memory);
                    for k = 1:length(mem_keys)
                        if ~to_module.memory(mem_keys{k})
                            all_high = false;
                            break;
                        end
                    end
                    pulse_to_send = double(~all_high);
                case 'broadcaster'
                    pulse_to_send = current_pulse.value;
                case 'other'
                    send_pulse = false;
            end

            if send_pulse
                modules(to_idx) = to_module; % Update module state
                for k = 1:length(to_module.destinations)
                    dest_name = to_module.destinations{k};
                    queue{queue_tail} = struct('value', pulse_to_send, 'from', to_module.name, 'to', dest_name);
                    queue_tail = queue_tail + 1;
                end
            end
        end
    end

    fprintf('%d\n', low_pulses * high_pulses);
end
