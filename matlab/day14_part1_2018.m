
function main()
    fileID = fopen('input.txt', 'r');
    input = fscanf(fileID, '%d');
    fclose(fileID);

    scoreboard = zeros(1, input + 12);
    scoreboard(1:2) = [3, 7];
    num_recipes = 2;
    elf1 = 1;
    elf2 = 2;

    while num_recipes < input + 10
        sum_val = scoreboard(elf1) + scoreboard(elf2);

        if sum_val >= 10
            scoreboard(num_recipes + 1) = 1;
            scoreboard(num_recipes + 2) = mod(sum_val, 10);
            num_recipes = num_recipes + 2;
        else
            scoreboard(num_recipes + 1) = sum_val;
            num_recipes = num_recipes + 1;
        end

        elf1 = mod(elf1 - 1 + 1 + scoreboard(elf1), num_recipes) + 1;
        elf2 = mod(elf2 - 1 + 1 + scoreboard(elf2), num_recipes) + 1;
    end

    result = scoreboard(input + 1 : input + 10);
    fprintf('%d', result);
    fprintf('\n');
end
