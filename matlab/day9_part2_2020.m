
function main()
    fid = fopen('input.txt', 'r');
    numbers = fscanf(fid, '%d');
    fclose(fid);

    invalid_num = 14360655;

    n = length(numbers);
    for i = 1:n
        total = numbers(i);
        for j = i + 1:n
            total = total + numbers(j);
            if total == invalid_num
                contiguous_set = numbers(i:j);
                weakness = min(contiguous_set) + max(contiguous_set);
                fprintf('%d\n', weakness);
                return;
            elseif total > invalid_num
                break;
            end
        end
    end
end
