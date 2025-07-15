
function main()
    fid = fopen('input.txt', 'r');

    totalCode = 0;
    totalMem = 0;

    line = fgetl(fid);
    while ischar(line)
        totalCode = totalCode + numel(line);

        content = line(2:end-1);
        content = regexprep(content, '\\\\', '_');
        content = regexprep(content, '\\"', '_');
        content = regexprep(content, '\\x[0-9a-f]{2}', '_');

        totalMem = totalMem + numel(content);

        line = fgetl(fid);
    end

    fclose(fid);

    fprintf('%d\n', totalCode - totalMem);
end
