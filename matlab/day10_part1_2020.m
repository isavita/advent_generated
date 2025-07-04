
function main()
    fid = fopen('input.txt', 'r');
    adapters = fscanf(fid, '%d');
    fclose(fid);

    adapters = sort(adapters);
    adapters = [0; adapters; adapters(end) + 3];

    diffs = diff(adapters);
    
    count1 = sum(diffs == 1);
    count3 = sum(diffs == 3);

    fprintf('%d\n', count1 * count3);
end

main();
