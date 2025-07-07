
function main()
    input_file = 'input.txt';
    paths = parse_input(input_file);
    cave = draw_cave(paths);
    result = simulate_sand(cave);
    disp(result);
end

function paths = parse_input(filename)
    fid = fopen(filename, 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};
    
    paths = cell(length(lines), 1);
    for i = 1:length(lines)
        points_str = strsplit(lines{i}, ' -> ');
        paths{i} = zeros(length(points_str), 2);
        for j = 1:length(points_str)
            coords = str2double(strsplit(points_str{j}, ','));
            paths{i}(j, :) = coords';
        end
    end
end

function cave = draw_cave(paths)
    cave = containers.Map('KeyType', 'char', 'ValueType', 'char');
    for i = 1:length(paths)
        path = paths{i};
        for j = 1:(size(path, 1) - 1)
            x1 = path(j, 1);
            y1 = path(j, 2);
            x2 = path(j+1, 1);
            y2 = path(j+1, 2);
            
            dx = sign(x2 - x1);
            dy = sign(y2 - y1);
            
            x = x1;
            y = y1;
            
            while x ~= x2 || y ~= y2
                cave_key = sprintf('%d,%d', x, y);
                cave(cave_key) = '#';
                if x ~= x2
                    x = x + dx;
                end
                if y ~= y2
                    y = y + dy;
                end
            end
            cave_key = sprintf('%d,%d', x2, y2);
            cave(cave_key) = '#';
        end
    end
end

function sand_count = simulate_sand(cave)
    sand_count = 0;
    source = [500, 0];
    
    while true
        x = source(1);
        y = source(2);
        
        while true
            if y >= 1000
                break;
            end
            
            next_pos_key_down = sprintf('%d,%d', x, y + 1);
            next_pos_key_down_left = sprintf('%d,%d', x - 1, y + 1);
            next_pos_key_down_right = sprintf('%d,%d', x + 1, y + 1);
            
            if ~isKey(cave, next_pos_key_down)
                y = y + 1;
            elseif ~isKey(cave, next_pos_key_down_left)
                x = x - 1;
                y = y + 1;
            elseif ~isKey(cave, next_pos_key_down_right)
                x = x + 1;
                y = y + 1;
            else
                break;
            end
        end
        
        if y >= 1000
            break;
        end
        
        cave_key = sprintf('%d,%d', x, y);
        cave(cave_key) = 'o';
        sand_count = sand_count + 1;
    end
end

main();
