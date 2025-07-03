
function main()
    fid = fopen('input.txt', 'r');
    stream = fscanf(fid, '%c');
    fclose(fid);
    stream = strtrim(stream');

    total_score = 0;
    current_score = 0;
    in_garbage = false;
    ignore_next = false;

    for i = 1:length(stream)
        char = stream(i);

        if ignore_next
            ignore_next = false;
        elseif char == '!'
            ignore_next = true;
        elseif char == '<' && ~in_garbage
            in_garbage = true;
        elseif char == '>' && in_garbage
            in_garbage = false;
        elseif char == '{' && ~in_garbage
            current_score = current_score + 1;
        elseif char == '}' && ~in_garbage
            total_score = total_score + current_score;
            current_score = current_score - 1;
        end
    end

    fprintf('%d\n', total_score);
end

main();
