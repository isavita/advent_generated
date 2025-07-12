
% solve.m

% --- Main entry point ---

% Initialize stacks as a cell array of strings (char arrays)
stacks = cell(1, 0);

fid = fopen('input.txt', 'r');
if fid == -1
    error('Error opening file input.txt');
end

% --- Part 1: Parse initial stack configuration ---
initial_config_lines = {};
while true
    line = fgetl(fid);
    if ~ischar(line)
        break;
    end
    if startsWith(strtrim(line), '1')
        break;
    end
    initial_config_lines{end+1} = line;
end

% Determine num_stacks from the line with numbers
num_stacks = (length(line) + 1) / 4;
stacks = cell(1, num_stacks);

% Populate stacks from initial_config_lines
% Iterate lines from bottom to top to build stacks correctly (bottom-most char first)
for i = length(initial_config_lines):-1:1
    current_line = initial_config_lines{i};
    for j = 1:num_stacks
        char_idx = (j-1)*4 + 2;
        if char_idx <= length(current_line) && isletter(current_line(char_idx))
            stacks{j} = [stacks{j}, current_line(char_idx)];
        end
    end
end

% Skip the blank line after stack numbers
fgetl(fid);

% --- Part 2: Process move instructions ---
while true
    line = fgetl(fid);
    if ~ischar(line)
        break;
    end

    % Parse move instruction
    tokens = textscan(line, 'move %d from %d to %d');
    n = tokens{1};
    from = tokens{2};
    to = tokens{3};

    % Perform the move (CrateMover 9001 logic: preserves order)
    crates_to_move = stacks{from}(end-n+1:end);
    stacks{from}(end-n+1:end) = [];
    stacks{to} = [stacks{to}, crates_to_move];
end

fclose(fid);

% --- Part 3: Print the top crate of each stack ---
result = '';
for i = 1:num_stacks
    if ~isempty(stacks{i})
        result = [result, stacks{i}(end)];
    end
end
disp(result);
