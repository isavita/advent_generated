
function main()
    fid = fopen('input.txt', 'r');
    h = 0;
    d = 0;
    while ~feof(fid)
        line = fgetl(fid);
        [action, valueStr] = strtok(line);
        value = str2double(valueStr);
        switch action
            case 'forward'
                h = h + value;
            case 'down'
                d = d + value;
            case 'up'
                d = d - value;
        end
    end
    fclose(fid);
    result = h * d;
    fprintf('%d\n', result);
end

main();
