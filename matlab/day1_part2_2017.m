
function main()
    fid = fopen('input.txt', 'r');
    data = fscanf(fid, '%c');
    fclose(fid);
    data = strtrim(data');

    total = 0;
    len_data = length(data);
    half_len = len_data / 2;

    for i = 1:len_data
        if data(i) == data(mod(i + half_len - 1, len_data) + 1)
            total = total + str2double(data(i));
        end
    end

    disp(total);
end

main();
