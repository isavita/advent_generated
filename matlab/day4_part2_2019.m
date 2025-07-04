
function main()
    fileId = fopen("input.txt", "r");
    data = fscanf(fileId, "%d-%d");
    fclose(fileId);
    start = data(1);
    end_val = data(2);

    count_part1 = 0;
    count_part2 = 0;

    for i = start:end_val
        num_str = num2str(i);
        
        adjacent = false;
        for j = 1:length(num_str) - 1
            if num_str(j) == num_str(j+1)
                adjacent = true;
                break;
            end
        end

        decreasing = false;
        for j = 1:length(num_str) - 1
            if str2double(num_str(j)) > str2double(num_str(j+1))
                decreasing = true;
                break;
            end
        end

        if adjacent && ~decreasing
            count_part1 = count_part1 + 1;
        end

        only_two_adjacent = false;
        count = 1;
        for j = 1:length(num_str) - 1
            if num_str(j) == num_str(j+1)
                count = count + 1;
            else
                if count == 2
                    only_two_adjacent = true;
                end
                count = 1;
            end
        end
        if count == 2
            only_two_adjacent = true;
        end

        if only_two_adjacent && ~decreasing
            count_part2 = count_part2 + 1;
        end
    end

    fprintf("%d\n", count_part1);
    fprintf("%d\n", count_part2);
end

main();
