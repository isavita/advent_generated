
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    openChars = containers.Map({'(', '[', '{', '<'}, {')', ']', '}', '>'});
    errorPoints = containers.Map({')', ']', '}', '>'}, {3, 57, 1197, 25137});
    autocompletePoints = containers.Map({')', ']', '}', '>'}, {1, 2, 3, 4});

    totalSyntaxErrorScore = 0;
    autocompleteScores = [];

    for i = 1:length(lines)
        line = lines{i};
        stack = {};
        isCorrupted = false;
        corruptedChar = '';

        for j = 1:length(line)
            char = line(j);
            if isKey(openChars, char)
                stack{end+1} = openChars(char);
            else
                if isempty(stack)
                    isCorrupted = true;
                    corruptedChar = char;
                    break;
                else
                    expected = stack{end};
                    stack(end) = [];
                    if ~strcmp(expected, char)
                        isCorrupted = true;
                        corruptedChar = char;
                        break;
                    end
                end
            end
        end

        if isCorrupted
            totalSyntaxErrorScore = totalSyntaxErrorScore + errorPoints(corruptedChar);
        else
            completionString = '';
            for k = length(stack):-1:1
                completionString = [completionString, stack{k}];
            end

            score = 0;
            for k = 1:length(completionString)
                score = score * 5 + autocompletePoints(completionString(k));
            end
            autocompleteScores(end+1) = score;
        end
    end

    disp(['Total Syntax Error Score: ', num2str(totalSyntaxErrorScore)]);

    autocompleteScores = sort(autocompleteScores);
    middleScore = autocompleteScores(floor(length(autocompleteScores) / 2) + 1);
    disp(['Middle Autocomplete Score: ', num2str(middleScore)]);
end

main();
