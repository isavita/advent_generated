
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%d-%d %c: %s', 'Delimiter', ' ');
    fclose(fid);

    policies = struct('min', num2cell(lines{1}), ...
                      'max', num2cell(lines{2}), ...
                      'letter', lines{3}, ...
                      'password', lines{4});

    validCountPartOne = 0;
    validCountPartTwo = 0;

    for i = 1:length(policies)
        policy = policies(i);
        password = policy.password;
        letter = policy.letter;
        minCount = policy.min;
        maxCount = policy.max;

        letterCount = sum(password == letter);
        if letterCount >= minCount && letterCount <= maxCount
            validCountPartOne = validCountPartOne + 1;
        end

        firstPosition = password(minCount) == letter;
        secondPosition = password(maxCount) == letter;
        if (firstPosition || secondPosition) && ~(firstPosition && secondPosition)
            validCountPartTwo = validCountPartTwo + 1;
        end
    end

    fprintf('Valid passwords (Part One): %d\n', validCountPartOne);
    fprintf('Valid passwords (Part Two): %d\n', validCountPartTwo);
end

main();
