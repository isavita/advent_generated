
function solve()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Could not open input.txt');
    end

    lines = {};
    maxLen = 0;
    tline = fgetl(fid);
    while ischar(tline)
        lines{end+1} = tline;
        maxLen = max(maxLen, length(tline));
        tline = fgetl(fid);
    end
    fclose(fid);

    rows = length(lines);
    cols = maxLen;

    grid = repmat(' ', rows, cols);
    for i = 1:rows
        line = lines{i};
        grid(i, 1:length(line)) = line;
    end

    y = 1;
    x = find(grid(y, :) == '|', 1);

    dx = 0;
    dy = 1;

    letters = '';

    while true
        if y < 1 || y > rows || x < 1 || x > cols
            break;
        end

        cellChar = grid(y, x);

        if cellChar == ' '
            break;
        end

        if cellChar >= 'A' && cellChar <= 'Z'
            letters(end+1) = cellChar;
        end

        if cellChar == '+'
            if dx == 0
                isLeftPath = (x > 1 && (grid(y, x-1) == '-' || (grid(y, x-1) >= 'A' && grid(y, x-1) <= 'Z')));
                if isLeftPath
                    dx = -1;
                else
                    dx = 1;
                end
                dy = 0;
            else
                isUpPath = (y > 1 && (grid(y-1, x) == '|' || (grid(y-1, x) >= 'A' && grid(y-1, x) <= 'Z')));
                if isUpPath
                    dy = -1;
                else
                    dy = 1;
                end
                dx = 0;
            end
        end

        x = x + dx;
        y = y + dy;
    end

    disp(letters);
end
