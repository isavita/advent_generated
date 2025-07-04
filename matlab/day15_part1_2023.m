
function main()
    fid = fopen('input.txt', 'r');
    data = fscanf(fid, '%s');
    fclose(fid);

    steps = strsplit(data, ',');

    total = 0;
    for i = 1:length(steps)
        step = steps{i};
        current_value = 0;
        for j = 1:length(step)
            current_value = mod( (current_value + double(step(j))) * 17, 256);
        end
        total = total + current_value;
    end

    fprintf('%d\n', total);
end

main();
