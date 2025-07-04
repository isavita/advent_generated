
function main()
    fid = fopen('input.txt', 'r');
    count = 0;
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        if isempty(line)
            continue;
        end
        pair = strsplit(line, ',');
        left = str2double(strsplit(pair{1}, '-'));
        right = str2double(strsplit(pair{2}, '-'));
        if (left(1) <= right(2) && left(2) >= right(1))
            count = count + 1;
        end
    end
    fclose(fid);
    disp(count);
end

main();
