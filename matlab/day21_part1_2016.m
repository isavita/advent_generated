
function scramble_password()
    fid = fopen('input.txt', 'r');
    operations = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    operations = operations{1};

    password = 'abcdefgh';

    for i = 1:length(operations)
        op = operations{i};
        password = apply_operation(op, password);
    end

    disp(password);
end

function password = apply_operation(op, password)
    fields = strsplit(op, ' ');
    
    switch fields{1}
        case 'swap'
            if strcmp(fields{2}, 'position')
                x = str2double(fields{3});
                y = str2double(fields{6});
                password = swap_position(password, x, y);
            elseif strcmp(fields{2}, 'letter')
                x = fields{3};
                y = fields{6};
                password = swap_letter(password, x, y);
            end
        case 'rotate'
            if strcmp(fields{2}, 'left')
                steps = str2double(fields{3});
                password = rotate_left(password, steps);
            elseif strcmp(fields{2}, 'right')
                steps = str2double(fields{3});
                password = rotate_right(password, steps);
            elseif strcmp(fields{2}, 'based')
                x = fields{7};
                password = rotate_based_on_position(password, x);
            end
        case 'reverse'
            x = str2double(fields{3});
            y = str2double(fields{5});
            password = reverse_positions(password, x, y);
        case 'move'
            x = str2double(fields{3});
            y = str2double(fields{6});
            password = move_position(password, x, y);
    end
end

function password = swap_position(password, x, y)
    if x > y
        temp = x;
        x = y;
        y = temp;
    end
    temp_char = password(x + 1);
    password(x + 1) = password(y + 1);
    password(y + 1) = temp_char;
end

function password = swap_letter(password, x, y)
    placeholder = '#';
    password = strrep(password, x, placeholder);
    password = strrep(password, y, x);
    password = strrep(password, placeholder, y);
end

function password = rotate_left(password, steps)
    len = length(password);
    steps = mod(steps, len);
    password = circshift(password, [0, -steps]);
end

function password = rotate_right(password, steps)
    len = length(password);
    steps = mod(steps, len);
    password = circshift(password, [0, steps]);
end

function password = rotate_based_on_position(password, x)
    idx = strfind(password, x);
    steps = idx;
    if idx >= 5
        steps = steps + 1;
    end
    password = rotate_right(password, steps);
end

function password = reverse_positions(password, x, y)
    if x > y
        temp = x;
        x = y;
        y = temp;
    end
    segment_to_reverse = password(x + 1 : y + 1);
    reversed_segment = fliplr(segment_to_reverse);
    password = [password(1 : x), reversed_segment, password(y + 2 : end)];
end

function password = move_position(password, x, y)
    char_to_move = password(x + 1);
    password_without_char = [password(1 : x), password(x + 2 : end)];
    password = [password_without_char(1 : y), char_to_move, password_without_char(y + 1 : end)];
end
