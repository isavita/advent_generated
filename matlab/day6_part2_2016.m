
% Read input file
fid = fopen('input.txt', 'r');
if fid == -1
    error('Could not open input.txt');
end

lines_cell = {};
tline = fgetl(fid);
while ischar(tline)
    lines_cell{end+1} = tline; %#ok<AGROW>
    tline = fgetl(fid);
end
fclose(fid);

% Handle empty input file
if isempty(lines_cell)
    error('Input file is empty.');
end

% Convert cell array of strings to a character matrix for efficient column access.
% This assumes all lines have the same length, as implied by the Python solution
% using len(lines[0]) and line[i].
char_matrix = char(lines_cell);

message_len = size(char_matrix, 2); % Number of columns
message1 = '';
message2 = '';

for i = 1:message_len
    column_chars = char_matrix(:, i);

    % Find unique characters and their counts
    [unique_chars, ~, ic] = unique(column_chars);
    counts = accumarray(ic, 1);

    % Find most common character
    [~, max_idx] = max(counts);
    most_common = unique_chars(max_idx);

    % Find least common character
    [~, min_idx] = min(counts);
    least_common = unique_chars(min_idx);

    message1 = [message1, most_common]; %#ok<AGROW>
    message2 = [message2, least_common]; %#ok<AGROW>
end

disp(message1);
disp(message2);
