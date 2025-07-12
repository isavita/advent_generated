
% main.m
function main()
    % Define constants
    maxRobots = 3;
    keyPad = [
        '789';
        '456';
        '123';
        ' 0A'
    ];
    robotPad = [
        ' ^A';
        '<v>'
    ];

    total_result = 0;

    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file input.txt');
    end

    while ~feof(fid)
        line = fgetl(fid);
        if ischar(line)
            code = strtrim(line);

            if isempty(code)
                continue;
            end

            numericPart = 0;
            digits_idx = (code >= '0' & code <= '9');
            if any(digits_idx)
                numericStr = code(digits_idx);
                numericPart = str2double(numericStr);
            end
            
            current_solve_value = solve(code, maxRobots, keyPad, robotPad, maxRobots);
            
            total_result = total_result + current_solve_value * numericPart;
        end
    end

    fclose(fid);
    fprintf('%d\n', total_result);
end

% findPosition.m
function [pos_i, pos_j] = findPosition(mat, ch)
    [rows, cols] = size(mat);
    for i = 1:rows
        for j = 1:cols
            if mat(i, j) == ch
                pos_i = i;
                pos_j = j;
                return;
            end
        end
    end
    pos_i = -1;
    pos_j = -1;
end

% ok.m
function result = ok(mat, st_i, st_j, seq)
    [rows, cols] = size(mat);
    curr_i = st_i;
    curr_j = st_j;

    result = true;

    for k = 1:length(seq)
        if mat(curr_i, curr_j) == ' '
            result = false;
            return;
        end

        move_char = seq(k);
        switch move_char
            case '^'
                curr_i = curr_i - 1;
            case 'v'
                curr_i = curr_i + 1;
            case '<'
                curr_j = curr_j - 1;
            case '>'
                curr_j = curr_j + 1;
        end

        if curr_i < 1 || curr_i > rows || curr_j < 1 || curr_j > cols
            result = false;
            return;
        end
    end
end

% generateMoves.m
function moves = generateMoves(pos_i, pos_j, objective, pad)
    [obj_i, obj_j] = findPosition(pad, objective);

    path1_moves = '';
    if pos_j > obj_j
        path1_moves = [path1_moves, repmat('<', 1, pos_j - obj_j)];
    end
    if pos_i > obj_i
        path1_moves = [path1_moves, repmat('^', 1, pos_i - obj_i)];
    end
    if pos_i < obj_i
        path1_moves = [path1_moves, repmat('v', 1, obj_i - pos_i)];
    end
    if pos_j < obj_j
        path1_moves = [path1_moves, repmat('>', 1, obj_j - pos_j)];
    end

    if ok(pad, pos_i, pos_j, path1_moves)
        moves = path1_moves;
        return;
    end

    path2_moves = '';
    if pos_j < obj_j
        path2_moves = [path2_moves, repmat('>', 1, obj_j - pos_j)];
    end
    if pos_i > obj_i
        path2_moves = [path2_moves, repmat('^', 1, pos_i - obj_i)];
    end
    if pos_i < obj_i
        path2_moves = [path2_moves, repmat('v', 1, obj_i - pos_i)];
    end
    if pos_j > obj_j
        path2_moves = [path2_moves, repmat('<', 1, pos_j - obj_j)];
    end
    
    moves = path2_moves;
end

% solve.m
function total_len = solve(code, robots, keyPad, robotPad, maxRobots)
    if robots <= 0
        total_len = length(code);
        return;
    end

    total_len = 0;
    
    if robots == maxRobots
        current_robot_pos_i = 4;
    else
        current_robot_pos_i = 1;
    end
    current_robot_pos_j = 3;

    for k = 1:length(code)
        ch = code(k);
        
        if robots == maxRobots
            moves_sequence = generateMoves(current_robot_pos_i, current_robot_pos_j, ch, keyPad);
            [char_pos_i_on_pad, char_pos_j_on_pad] = findPosition(keyPad, ch);
        else
            moves_sequence = generateMoves(current_robot_pos_i, current_robot_pos_j, ch, robotPad);
            [char_pos_i_on_pad, char_pos_j_on_pad] = findPosition(robotPad, ch);
        end
        
        current_robot_pos_i = char_pos_i_on_pad;
        current_robot_pos_j = char_pos_j_on_pad;

        newCode_for_next_robot = [moves_sequence, 'A'];
        
        total_len = total_len + solve(newCode_for_next_robot, robots - 1, keyPad, robotPad, maxRobots);
    end
end
