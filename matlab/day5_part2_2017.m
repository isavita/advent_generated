
function main()
    fid = fopen('input.txt', 'r');
    jumps = fscanf(fid, '%d');
    fclose(fid);

    index = 1;
    steps = 0;
    n = length(jumps);

    while index >= 1 && index <= n
        offset = jumps(index);
        if offset < 3
            jumps(index) = jumps(index) + 1;
        else
            jumps(index) = jumps(index) - 1;
        end
        index = index + offset;
        steps = steps + 1;
    end

    fprintf('%d\n', steps);
end

main();
