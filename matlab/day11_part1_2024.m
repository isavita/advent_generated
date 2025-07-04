
function main()
    fid = fopen('input.txt', 'r');
    line = fgetl(fid);
    fclose(fid);
    stones = strsplit(line);

    for i = 1:25
        next_stones = {};
        for j = 1:length(stones)
            s = stones{j};
            if strcmp(s, '0')
                next_stones{end+1} = '1';
            elseif mod(length(s), 2) == 0
                mid = floor(length(s) / 2);
                left = trim_leading_zeros(s(1:mid));
                right = trim_leading_zeros(s(mid+1:end));
                next_stones{end+1} = left;
                next_stones{end+1} = right;
            else
                next_stones{end+1} = num2str(str2double(s) * 2024);
            end
        end
        stones = next_stones;
    end

    fprintf('%d\n', length(stones));
end

function trimmed_s = trim_leading_zeros(s)
    trimmed_s = regexprep(s, '^0+', '');
    if isempty(trimmed_s)
        trimmed_s = '0';
    end
end

main();
