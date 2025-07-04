
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%d');
    fclose(fid);

    data = cell2mat(data);

    if isempty(data)
        disp(0);
        return;
    end

    diffs = diff(data);
    count = sum(diffs > 0);

    disp(count);
end

main();
