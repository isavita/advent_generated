
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end

    MAX_R = 200;
    MAX_C = 1000;
    EXTRA_SPACE = 250;
    START_COL_ORIGINAL = 500;

    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);

    lines = lines{1};

    min_c_val = inf;
    max_r_val = 0;
    max_c_val = 0;

    for i = 1:length(lines)
        parts = strsplit(lines{i}, ' -> ');
        for j = 1:length(parts)
            coords = sscanf(parts{j}, '%d,%d');
            if ~isempty(coords)
                c = coords(1);
                r = coords(2);
                min_c_val = min(min_c_val, c);
                max_r_val = max(max_r_val, r);
                max_c_val = max(max_c_val, c);
            end
        end
    end

    matrix = repmat('.', MAX_R, MAX_C);
    offset_c = EXTRA_SPACE;
    
    for i = 1:length(lines)
        parts = strsplit(lines{i}, ' -> ');
        for j = 1:length(parts) - 1
            coords1 = sscanf(parts{j}, '%d,%d');
            coords2 = sscanf(parts{j+1}, '%d,%d');
            
            r1 = coords1(2);
            c1 = coords1(1) - min_c_val + offset_c;
            r2 = coords2(2);
            c2 = coords2(1) - min_c_val + offset_c;

            if r1 == r2
                start_c = min(c1, c2);
                end_c = max(c1, c2);
                matrix(r1+1, start_c+1:end_c+1) = '#';
            elseif c1 == c2
                start_r = min(r1, r2);
                end_r = max(r1, r2);
                matrix(start_r+1:end_r+1, c1+1) = '#';
            end
        end
    end

    floor_r = max_r_val + 2;
    matrix(floor_r+1, :) = '#';

    origin_c = START_COL_ORIGINAL - min_c_val + offset_c;
    
    sand_count = 0;
    while true
        r = 0;
        c = origin_c;
        
        if matrix(r+1, c+1) == 'o'
            break;
        end

        moved = false;
        while r < MAX_R - 1
            if matrix(r+2, c+1) == '.'
                r = r + 1;
            elseif c > 0 && matrix(r+2, c) == '.'
                r = r + 1;
                c = c - 1;
            elseif c < MAX_C - 1 && matrix(r+2, c+2) == '.'
                r = r + 1;
                c = c + 1;
            else
                matrix(r+1, c+1) = 'o';
                sand_count = sand_count + 1;
                moved = true;
                break;
            end
        end
        if ~moved
            break;
        end
    end

    fprintf('%d\n', sand_count);
end
