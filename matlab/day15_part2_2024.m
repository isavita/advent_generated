
function main()
    input_str = fileread('input.txt');

    result1 = solve(input_str);
    fprintf('%d\n', result1);

    scaled_input = scale_up(input_str);
    result2 = solve(scaled_input);
    fprintf('%d\n', result2);
end

function score = solve(input_str)
    [map, robot_pos, steps] = parse_input(input_str);
    [height, width] = size(map);

    for i = 1:size(steps, 1)
        dir = steps(i, :);
        next_robot_pos = robot_pos + dir;

        if next_robot_pos(1) < 1 || next_robot_pos(1) > height || ...
           next_robot_pos(2) < 1 || next_robot_pos(2) > width
            continue;
        end
        
        if map(next_robot_pos(1), next_robot_pos(2)) == '#'
            continue;
        end

        [success, next_map] = try_to_step(map, next_robot_pos, dir);
        
        if success
            next_map(next_robot_pos(1), next_robot_pos(2)) = '@';
            next_map(robot_pos(1), robot_pos(2)) = '.';
            map = next_map;
            robot_pos = next_robot_pos;
        end
    end

    score = calculate_score(map);
end

function [map, robot_pos, steps] = parse_input(input_str)
    parts = strsplit(strtrim(input_str), sprintf('\n\n'));
    map_lines = strsplit(parts{1}, '\n');
    
    width = max(cellfun('length', map_lines));
    map = char(cellfun(@(s) [s, repmat(' ', 1, width-length(s))], map_lines, 'UniformOutput', false));
    
    [r, c] = find(map == '@', 1);
    robot_pos = [r, c];
    
    steps_str = parts{2};
    steps = zeros(0, 2);
    for c = steps_str
        switch c
            case '^', dir = [-1, 0];
            case 'v', dir = [1, 0];
            case '<', dir = [0, -1];
            case '>', dir = [0, 1];
            otherwise, continue;
        end
        steps(end+1, :) = dir;
    end
end

function scaled_str = scale_up(input_str)
    parts = strsplit(input_str, sprintf('\n\n'));
    map_str = parts{1};
    steps_str = parts{2};
    
    scaled_map_str = '';
    for c = map_str
        switch c
            case '#', scaled_map_str = [scaled_map_str, '##'];
            case '.', scaled_map_str = [scaled_map_str, '..'];
            case 'O', scaled_map_str = [scaled_map_str, '[]'];
            case '@', scaled_map_str = [scaled_map_str, '@.'];
            otherwise, scaled_map_str = [scaled_map_str, c];
        end
    end
    
    scaled_str = [scaled_map_str, sprintf('\n\n'), steps_str];
end

function score = calculate_score(map)
    [rows, cols] = find(map == '[' | map == 'O');
    if isempty(rows)
        score = 0;
    else
        score = sum((cols - 1) + 100 * (rows - 1));
    end
end

function [success, new_map] = try_to_step(current_map, pos, dir)
    [height, width] = size(current_map);
    new_map = current_map;
    success = false;

    destination = pos + dir;
    if destination(1) < 1 || destination(1) > height || ...
       destination(2) < 1 || destination(2) > width
        return;
    end

    thing_to_move = current_map(pos(1), pos(2));
    switch thing_to_move
        case '.'
            success = true;
        case '#'
            return;
        case {'O', '@'}
            [s, m] = try_to_step(current_map, destination, dir);
            if s
                m(destination(1), destination(2)) = thing_to_move;
                m(pos(1), pos(2)) = '.';
                new_map = m;
                success = true;
            end
        case '['
            right_part = pos + [0, 1];
            if right_part(2) > width || current_map(right_part(1), right_part(2)) ~= ']'
                return;
            end
            
            if dir(1) == 0 % Horizontal push
                if dir(2) == -1 % Left
                    [s, m] = try_to_step(current_map, pos + dir, dir);
                    if s
                        m(pos(1), pos(2) - 1) = '[';
                        m(pos(1), pos(2)) = ']';
                        m(right_part(1), right_part(2)) = '.';
                        new_map = m;
                        success = true;
                    end
                else % Right
                    [s, m] = try_to_step(current_map, right_part + dir, dir);
                    if s
                        m(right_part(1), right_part(2) + 1) = ']';
                        m(right_part(1), right_part(2)) = '[';
                        m(pos(1), pos(2)) = '.';
                        new_map = m;
                        success = true;
                    end
                end
            else % Vertical push
                [s1, m1] = try_to_step(current_map, pos + dir, dir);
                if s1
                    [s2, m2] = try_to_step(m1, right_part + dir, dir);
                    if s2
                        m2(pos(1) + dir(1), pos(2) + dir(2)) = '[';
                        m2(right_part(1) + dir(1), right_part(2) + dir(2)) = ']';
                        m2(pos(1), pos(2)) = '.';
                        m2(right_part(1), right_part(2)) = '.';
                        new_map = m2;
                        success = true;
                    end
                end
            end
        case ']'
            left_part = pos - [0, 1];
            if left_part(2) >= 1 && current_map(left_part(1), left_part(2)) == '['
                [success, new_map] = try_to_step(current_map, left_part, dir);
            end
    end
end
