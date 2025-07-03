
function main()
    fid = fopen('input.txt', 'r');
    num_elves = fscanf(fid, '%d');
    fclose(fid);

    result = josephus(num_elves);
    fprintf('%d\n', result);
end

function result = josephus(n)
    i = 1;
    while i * 3 <= n
        i = i * 3;
    end
    result = n - i + max(n - 2 * i, 0);
end

main();
