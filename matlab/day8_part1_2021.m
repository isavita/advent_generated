
function main()
    fid = fopen('input.txt', 'r');
    count = 0;
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        parts = strsplit(line, ' | ');
        output = parts{2};
        digits = strsplit(output, ' ');
        for i = 1:length(digits)
            len = length(digits{i});
            if len == 2 || len == 4 || len == 3 || len == 7
                count = count + 1;
            end
        end
    end
    fclose(fid);
    disp(count);
end

main();
