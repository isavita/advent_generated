
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    data = data{1};

    total = 0;
    for i = 1:length(data)
        line = data{i};
        len = length(line);
        first_half = line(1:floor(len/2));
        second_half = line(floor(len/2)+1:end);

        common_items = intersect(first_half, second_half);

        for j = 1:length(common_items)
            item = common_items(j);
            if islower(item)
                total = total + (double(item) - 96);
            else
                total = total + (double(item) - 38);
            end
        end
    end

    fprintf('%d\n', total);
end

main();
