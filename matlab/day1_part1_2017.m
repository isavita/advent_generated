
function main()
    fid = fopen('input.txt', 'r');
    data = fscanf(fid, '%c');
    fclose(fid);

    data = strtrim(data);

    total = 0;
    n = length(data);
    for i = 1:n
        if data(i) == data(mod(i, n) + 1)
            total = total + str2double(data(i));
        end
    end

    disp(total);
end

main();
