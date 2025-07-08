
function main()
    fid = fopen('input.txt', 'r');
    instructions = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    instructions = instructions{1};
    fclose(fid);

    hashed = 'fbgdceah';
    result = unscramble(hashed, instructions);
    disp(result);
end

function scrambled_pw = scramble(pw, instructions, direction)
    if direction < 0
        instructions = instructions(end:-1:1);
    end

    for i = 1:numel(instructions)
        line = strsplit(instructions{i}, ' ');
        
        if strcmp(line{1}, 'swap')
            if strcmp(line{2}, 'position')
                x = str2double(line{3});
                y = str2double(line{end});
                pw = swap_positions(pw, x, y);
            else % swap letter
                x = line{3}(1);
                y = line{end}(1);
                pw = swap_letters(pw, x, y);
            end
        elseif strcmp(line{1}, 'rotate')
            if strcmp(line{2}, 'based')
                if direction > 0
                    pw = rotate_letter(pw, line{end}(1));
                else
                    pw = derotate_letter(pw, line{end}(1));
                end
            else % rotate left/right
                x = str2double(line{3});
                if strcmp(line{2}, 'left')
                    x = -x;
                end
                if direction < 0
                    x = -x;
                end
                pw = rotate(pw, x);
            end
        elseif strcmp(line{1}, 'reverse')
            x = str2double(line{3});
            y = str2double(line{end});
            pw = reverse_segment(pw, x, y);
        elseif strcmp(line{1}, 'move')
            x = str2double(line{3});
            y = str2double(line{end});
            if direction < 0
                temp = x;
                x = y;
                y = temp;
            end
            pw = move_char(pw, x, y);
        end
    end
    scrambled_pw = pw;
end

function pw = swap_positions(pw, x, y)
    temp = pw(x+1);
    pw(x+1) = pw(y+1);
    pw(y+1) = temp;
end

function pw = swap_letters(pw, x, y)
    idx_x = find(pw == x, 1);
    idx_y = find(pw == y, 1);
    pw = swap_positions(pw, idx_x-1, idx_y-1);
end

function pw = rotate(pw, steps)
    pw = circshift(pw, steps);
end

function pw = rotate_letter(pw, x)
    idx = find(pw == x, 1);
    steps = idx + 1;
    if idx >= 5
        steps = steps + 1;
    end
    pw = rotate(pw, steps);
end

function pw = derotate_letter(pw, x)
    idx = find(pw == x, 1);
    
    if mod(idx, 2) == 0
        rot = -floor(idx / 2);
    elseif idx ~= 1
        rot = floor((7 - idx) / 2);
    else
        rot = -1;
    end
    pw = rotate(pw, rot);
end

function pw = reverse_segment(pw, x, y)
    segment = pw(x+1:y+1);
    pw(x+1:y+1) = segment(end:-1:1);
end

function pw = move_char(pw, x, y)
    ch = pw(x+1);
    pw(x+1) = [];
    
    if y == 0
        pw = [ch, pw];
    elseif y == numel(pw)
        pw = [pw, ch];
    else
        pw = [pw(1:y), ch, pw(y+1:end)];
    end
end

function unscrambled_pw = unscramble(pw, instructions)
    unscrambled_pw = scramble(pw, instructions, -1);
end
