
function main()
    fid = fopen('input.txt', 'r');
    steps = fscanf(fid, '%d');
    fclose(fid);

    currentPos = 0;
    valueAfterZero = 0;

    for i = 1:50000000
        currentPos = mod(currentPos + steps, i);
        if currentPos == 0
            valueAfterZero = i;
        end
        currentPos = currentPos + 1;
    end

    fprintf('%d\n', valueAfterZero);
end
