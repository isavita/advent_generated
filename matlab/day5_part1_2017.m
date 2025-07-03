
function main()
    fid = fopen('input.txt', 'r');
    jumps = fscanf(fid, '%d');
    fclose(fid);

    currentIndex = 0;
    steps = 0;

    while currentIndex >= 0 && currentIndex < length(jumps)
        nextIndex = currentIndex + jumps(currentIndex + 1);
        jumps(currentIndex + 1) = jumps(currentIndex + 1) + 1;
        currentIndex = nextIndex;
        steps = steps + 1;
    end

    fprintf('%d\n', steps);
end

main();
