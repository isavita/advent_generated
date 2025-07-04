
function main()
    h = 0;
    d = 0;
    a = 0;

    fileId = fopen('input.txt', 'r');
    if fileId == -1
        error('Unable to open input.txt');
    end

    while ~feof(fileId)
        line = strtrim(fgetl(fileId));
        if isempty(line)
            continue;
        end
        parts = strsplit(line);
        command = parts{1};
        value = str2double(parts{2});

        switch command
            case 'forward'
                h = h + value;
                d = d + a * value;
            case 'down'
                a = a + value;
            case 'up'
                a = a - value;
        end
    end

    fclose(fileId);
    fprintf('%d\n', h * d);
end

main();
