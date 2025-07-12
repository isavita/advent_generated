
function main()
    global memoMap;
    global MAX_ROBOTS;
    global key_pad;
    global key_pad_rows;
    global key_pad_cols;
    global robot_pad;
    global robot_pad_rows;
    global robot_pad_cols;

    MAX_ROBOTS = 26;
    key_pad = {'789'; '456'; '123'; ' 0A'};
    key_pad_rows = 4;
    key_pad_cols = 3;

    robot_pad = {' ^A'; '<v>'};
    robot_pad_rows = 2;
    robot_pad_cols = 3;

    memoMap = containers.Map();

    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Could not open input.txt');
    end

    total_ret = int64(0);

    while ~feof(fid)
        line = fgetl(fid);
        if ~ischar(line)
            break;
        end
        
        line = strtrim(line);
        if isempty(line)
            continue;
        end

        numeric_part = int64(0);
        code_str = '';
        for i = 1:length(line)
            char_val = line(i);
            if ismember(char_val, '0123456789')
                numeric_part = numeric_part * 10 + int64(char_val - '0');
            end
            code_str = [code_str, char_val];
        end

        if numeric_part > 0
            total_ret = total_ret + solve(code_str, MAX_ROBOTS) * numeric_part;
        end
    end

    fclose(fid);
    fprintf('%s\n', num2str(total_ret));
end

function ret = solve(code, robots)
    global memoMap;
    global MAX_ROBOTS;
    global key_pad;
    global key_pad_rows;
    global key_pad_cols;
    global robot_pad;
    global robot_pad_rows;
    global robot_pad_cols;
    
    memo_key = sprintf('%s_%d', code, robots);
    if memoMap.isKey(memo_key)
        ret = memoMap(memo_key);
        return;
    end

    if robots <= 0
        ret = int64(length(code));
        memoMap(memo_key) = ret;
        return;
    end

    current_r = 0;
    current_c = 0;
    
    if robots == MAX_ROBOTS
        current_r = 3; current_c = 2;
        pad = key_pad;
        pad_rows = key_pad_rows;
        pad_cols = key_pad_cols;
    else
        current_r = 0; current_c = 2;
        pad = robot_pad;
        pad_rows = robot_pad_rows;
        pad_cols = robot_pad_cols;
    end

    ret = int64(0);
    for i = 1:length(code)
        ch = code(i);
        
        moves_to_char = generate_moves(current_r, current_c, ch, pad, pad_rows, pad_cols);
        
        [current_r, current_c] = find_position(pad, ch);
        
        full_moves_seq = [moves_to_char, 'A'];
        ret = ret + solve(full_moves_seq, robots - 1);
    end

    memoMap(memo_key) = ret;
end

function [r, c] = find_position(mat, ch)
    r = -1; c = -1;
    for i = 1:size(mat, 1)
        row_str = mat{i};
        idx = strfind(row_str, ch);
        if ~isempty(idx)
            r = i - 1;
            c = idx(1) - 1;
            return;
        end
    end
end

function ok = is_ok(mat, rows, cols, r_start, c_start, seq)
    r = r_start;
    c = c_start;
    ok = true;
    for i = 1:length(seq)
        if r < 0 || r >= rows || c < 0 || c >= cols || mat{r+1}(c+1) == ' '
            ok = false;
            return;
        end
        move_char = seq(i);
        switch move_char
            case '^'
                r = r - 1;
            case 'v'
                r = r + 1;
            case '<'
                c = c - 1;
            case '>'
                c = c + 1;
        end
    end
end

function moves = generate_moves(r_start, c_start, objective, pad, pad_rows, pad_cols)
    [obj_r, obj_c] = find_position(pad, objective);

    p1_moves = '';
    if c_start > obj_c, p1_moves = [p1_moves, repmat('<', 1, c_start - obj_c)]; end
    if r_start > obj_r, p1_moves = [p1_moves, repmat('^', 1, r_start - obj_r)]; end
    if r_start < obj_r, p1_moves = [p1_moves, repmat('v', 1, obj_r - r_start)]; end
    if c_start < obj_c, p1_moves = [p1_moves, repmat('>', 1, obj_c - c_start)]; end

    if is_ok(pad, pad_rows, pad_cols, r_start, c_start, p1_moves)
        moves = p1_moves;
        return;
    end

    p2_moves = '';
    if c_start < obj_c, p2_moves = [p2_moves, repmat('>', 1, obj_c - c_start)]; end
    if r_start > obj_r, p2_moves = [p2_moves, repmat('^', 1, r_start - obj_r)]; end
    if r_start < obj_r, p2_moves = [p2_moves, repmat('v', 1, obj_r - r_start)]; end
    if c_start > obj_c, p2_moves = [p2_moves, repmat('<', 1, c_start - obj_c)]; end

    moves = p2_moves;
end
