
function main()
    nums = [];
    file = fopen("input.txt", "r");
    while ~feof(file)
        line = fgetl(file);
        nums = [nums; struct('pos', length(nums), 'val', str2double(line))];
    end
    fclose(file);

    nums2 = nums;
    for i = 1:length(nums2)
        nums2(i).val = 811589153 * nums2(i).val;
    end

    mixed_nums = mix(nums);
    disp(coords(mixed_nums));
end

function mixed_nums = mix(nums)
    n = length(nums);
    for i = 1:n
        oldpos = nums(i).pos;
        newpos = mod(oldpos + nums(i).val, n - 1);
        
        if oldpos < newpos
            for j = 1:n
                if nums(j).pos > oldpos && nums(j).pos <= newpos
                    nums(j).pos = nums(j).pos - 1;
                end
            end
        elseif newpos < oldpos
            for j = 1:n
                if nums(j).pos >= newpos && nums(j).pos < oldpos
                    nums(j).pos = nums(j).pos + 1;
                end
            end
        end
        nums(i).pos = newpos;
    end
    mixed_nums = nums;
end

function sum_val = coords(nums)
    l = length(nums);
    zeroPos = -1;
    for i = 1:l
        if nums(i).val == 0
            zeroPos = nums(i).pos;
            break;
        end
    end
    
    sum_val = 0;
    indices_to_sum = [mod(zeroPos + 1000, l), mod(zeroPos + 2000, l), mod(zeroPos + 3000, l)];
    
    for i = 1:l
        if ismember(nums(i).pos, indices_to_sum)
            sum_val = sum_val + nums(i).val;
        end
    end
end

main();
