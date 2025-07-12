
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end
    directions = textscan(fid, '%s', 'Delimiter', '', 'CollectOutput', true);
    fclose(fid);

    directions = directions{1}{1};
    directions = directions(2:end-1);

    rooms = containers.Map('KeyType', 'char', 'ValueType', 'double');
    stack = {};

    current_pos = [0, 0];
    rooms_str = sprintf('%d,%d', current_pos(1), current_pos(2));
    rooms(rooms_str) = 0;

    i = 1;
    while i <= length(directions)
        char_dir = directions(i);
        switch char_dir
            case '('
                stack{end+1} = current_pos;
            case '|'
                current_pos = stack{end};
            case ')'
                current_pos = stack{end};
                stack(end) = [];
            case {'N', 'E', 'S', 'W'}
                dx = 0; dy = 0;
                if char_dir == 'N'
                    dy = -1;
                elseif char_dir == 'E'
                    dx = 1;
                elseif char_dir == 'S'
                    dy = 1;
                elseif char_dir == 'W'
                    dx = -1;
                end

                next_pos = current_pos + [dx, dy];
                rooms_str_current = sprintf('%d,%d', current_pos(1), current_pos(2));
                rooms_str_next = sprintf('%d,%d', next_pos(1), next_pos(2));

                current_doors = rooms(rooms_str_current);
                if isKey(rooms, rooms_str_next)
                    next_doors = rooms(rooms_str_next);
                    if current_doors + 1 < next_doors
                        rooms(rooms_str_next) = current_doors + 1;
                    end
                else
                    rooms(rooms_str_next) = current_doors + 1;
                end
                current_pos = next_pos;
        end
        i = i + 1;
    end

    max_doors = 0;
    rooms_with_1000_doors = 0;
    keys_rooms = keys(rooms);
    for k = 1:length(keys_rooms)
        doors = rooms(keys_rooms{k});
        if doors > max_doors
            max_doors = doors;
        end
        if doors >= 1000
            rooms_with_1000_doors = rooms_with_1000_doors + 1;
        end
    end

    fprintf('%d\n', max_doors);
    fprintf('%d\n', rooms_with_1000_doors);
end
