
function main()
    x = [1];
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    for i = 1:length(lines)
        line = lines{i};
        if strcmp(line, 'noop')
            x(end+1) = x(end);
        else
            parts = strsplit(line);
            n = str2double(parts{2});
            x(end+1) = x(end);
            x(end+1) = x(end) + n;
        end
    end

    indices = 19:40:length(x)-1;
    sum_val = sum((indices + 1) .* x(indices + 1));

    disp(sum_val);
end

main();
