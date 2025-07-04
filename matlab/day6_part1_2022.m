
function main()
    fid = fopen('input.txt', 'r');
    data = fscanf(fid, '%s');
    fclose(fid);

    for i = 4:length(data)
        if numel(unique(data(i-3:i))) == 4
            fprintf('%d\n', i);
            return;
        end
    end
end

main();
