
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Could not open file');
    end

    keypad = containers.Map('KeyType', 'char', 'ValueType', 'char');
    keypad('1') = '1'; keypad('2') = '2'; keypad('3') = '3';
    keypad('4') = '4'; keypad('5') = '5'; keypad('6') = '6';
    keypad('7') = '7'; keypad('8') = '8'; keypad('9') = '9';
    keypad('A') = 'A'; keypad('B') = 'B'; keypad('C') = 'C';
    keypad('D') = 'D';

    moves = containers.Map('KeyType', 'char', 'ValueType', 'any');
    moves('1') = struct('U', '1', 'D', '3');
    moves('2') = struct('R', '3', 'D', '6');
    moves('3') = struct('U', '1', 'R', '4', 'D', '7', 'L', '2');
    moves('4') = struct('L', '3', 'D', '8');
    moves('5') = struct('R', '6');
    moves('6') = struct('U', '2', 'R', '7', 'D', 'A', 'L', '5');
    moves('7') = struct('U', '3', 'R', '8', 'D', 'B', 'L', '6');
    moves('8') = struct('U', '4', 'R', '9', 'D', 'C', 'L', '7');
    moves('9') = struct('L', '8');
    moves('A') = struct('U', '6', 'R', 'B');
    moves('B') = struct('U', '7', 'R', 'C', 'D', 'D', 'L', 'A');
    moves('C') = struct('U', '8', 'L', 'B');
    moves('D') = struct('U', 'B');

    position = '5';
    code = '';

    while ~feof(fid)
        instruction = strtrim(fgetl(fid));
        for i = 1:length(instruction)
            move = instruction(i);
            currentMoves = moves(position);
            if isfield(currentMoves, move)
                position = currentMoves.(move);
            end
        end
        code = [code, position];
    end

    fclose(fid);
    disp(code);
end
