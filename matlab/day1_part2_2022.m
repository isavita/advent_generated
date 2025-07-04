
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    data = data{1};

    calories = zeros(1, 1000); % Pre-allocate for potential elves
    current_elf = 1;
    for i = 1:length(data)
        if isempty(data{i})
            current_elf = current_elf + 1;
        else
            calories(current_elf) = calories(current_elf) + str2double(data{i});
        end
    end

    calories = calories(calories > 0); % Remove unused pre-allocated zeros
    sorted_calories = sort(calories, 'descend');
    top_three = sorted_calories(1:3);

    disp(sum(top_three));
end
