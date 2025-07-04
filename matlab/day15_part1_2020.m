
function main()
    fid = fopen('input.txt', 'r');
    line = fscanf(fid, '%s');
    fclose(fid);
    
    starting_numbers = str2num(line);
    
    last_seen = containers.Map('KeyType', 'double', 'ValueType', 'double');
    for i = 1:(length(starting_numbers) - 1)
        last_seen(starting_numbers(i)) = i;
    end
    
    last_num = starting_numbers(end);
    for i = length(starting_numbers):2019
        if isKey(last_seen, last_num)
            new_num = i - last_seen(last_num);
        else
            new_num = 0;
        end
        last_seen(last_num) = i;
        last_num = new_num;
    end
    
    fprintf('%d\n', last_num);
end

main();
