
function main()
    MAX_VALVES = 60;
    MAX_TIME = 30;
    INF = 1e6;

    valve_map = containers.Map('KeyType', 'char', 'ValueType', 'double');
    valves = struct('id', {}, 'flow', {}, 'useful_idx', {});
    num_valves = 0;
    dist = ones(MAX_VALVES) * INF;
    for i = 1:MAX_VALVES, dist(i, i) = 0; end

    fid = fopen('input.txt', 'r');
    while ~feof(fid)
        line = fgetl(fid);
        if isempty(line), continue; end
        
        tokens = regexp(line, 'Valve (\w{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)', 'tokens');
        valve_id = tokens{1}{1};
        flow = str2double(tokens{1}{2});
        tunnels_str = tokens{1}{3};
        
        if ~isKey(valve_map, valve_id)
            num_valves = num_valves + 1;
            valve_map(valve_id) = num_valves;
            valves(num_valves).id = valve_id;
        end
        u_idx = valve_map(valve_id);
        valves(u_idx).flow = flow;

        tunnel_ids = strsplit(tunnels_str, ', ');
        for i = 1:length(tunnel_ids)
            v_id = tunnel_ids{i};
            if ~isKey(valve_map, v_id)
                num_valves = num_valves + 1;
                valve_map(v_id) = num_valves;
                valves(num_valves).id = v_id;
            end
            v_idx = valve_map(v_id);
            dist(u_idx, v_idx) = 1;
        end
    end
    fclose(fid);

    dist = dist(1:num_valves, 1:num_valves);
    valves = valves(1:num_valves);

    for k = 1:num_valves
        for i = 1:num_valves
            for j = 1:num_valves
                dist(i, j) = min(dist(i, j), dist(i, k) + dist(k, j));
            end
        end
    end

    useful_indices = find([valves.flow] > 0);
    num_useful = length(useful_indices);
    for i = 1:num_useful
        valves(useful_indices(i)).useful_idx = i;
    end

    start_idx = valve_map('AA');
    initial_mask = bitshift(1, num_useful) - 1;
    memo = ones(num_valves, MAX_TIME + 1, bitshift(1, num_useful), 'int64') * -1;

    result = solve_recursive(start_idx, MAX_TIME, initial_mask);
    fprintf('%d\n', result);

    function max_p = solve_recursive(u_idx, time_left, mask)
        if time_left <= 0
            max_p = int64(0);
            return;
        end
        
        if memo(u_idx, time_left + 1, mask + 1) ~= -1
            max_p = memo(u_idx, time_left + 1, mask + 1);
            return;
        end

        max_p = int64(0);
        
        for i = 1:num_useful
            if bitget(mask, i)
                v_idx = useful_indices(i);
                time_needed = dist(u_idx, v_idx) + 1;

                if time_left > time_needed
                    next_time = time_left - time_needed;
                    current_release = int64(valves(v_idx).flow) * int64(next_time);
                    next_mask = bitset(mask, i, 0);
                    
                    max_p = max(max_p, current_release + solve_recursive(v_idx, next_time, next_mask));
                end
            end
        end
        
        memo(u_idx, time_left + 1, mask + 1) = max_p;
    end
end
