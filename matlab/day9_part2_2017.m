
function main()
    fid = fopen('input.txt', 'r');
    data = fscanf(fid, '%c');
    fclose(fid);
    data = strtrim(data');

    score = 0;
    garbage_count = 0;
    in_garbage = false;
    ignore_next = false;

    for i = 1:length(data)
        char = data(i);
        if ignore_next
            ignore_next = false;
        elseif char == '!'
            ignore_next = true;
        elseif in_garbage
            if char == '>'
                in_garbage = false;
            else
                garbage_count = garbage_count + 1;
            end
        elseif char == '<'
            in_garbage = true;
        elseif char == '{'
            score = score + 1;
        end
    end

    fprintf('%d\n', score);
    fprintf('%d\n', garbage_count);
end

main();
