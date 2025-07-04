
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    illegal_chars = containers.Map({')', ']', '}', '>'}, {3, 57, 1197, 25137});
    total_score = 0;

    for i = 1:length(lines)
        line = lines{i};
        stack = {};
        for j = 1:length(line)
            char = line(j);
            if ismember(char, {'(', '[', '{', '<'})
                stack{end+1} = char;
            elseif ismember(char, {')', ']', '}', '>'})
                if isempty(stack) || ~((strcmp(char, ')') && strcmp(stack{end}, '(')) || ...
                                       (strcmp(char, ']') && strcmp(stack{end}, '[')) || ...
                                       (strcmp(char, '}') && strcmp(stack{end}, '{')) || ...
                                       (strcmp(char, '>') && strcmp(stack{end}, '<')))
                    total_score = total_score + illegal_chars(char);
                    break;
                else
                    stack(end) = [];
                end
            end
        end
    end

    fprintf('%d\n', total_score);
end
