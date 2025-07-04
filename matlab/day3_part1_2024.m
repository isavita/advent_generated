
function main()
    fid = fopen('input.txt', 'r');
    total_sum = 0;
    while ~feof(fid)
        line = fgetl(fid);
        matches = regexp(line, 'mul\s*\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*\)', 'tokens');
        for i = 1:length(matches)
            num1 = str2double(matches{i}{1});
            num2 = str2double(matches{i}{2});
            total_sum = total_sum + num1 * num2;
        end
    end
    fclose(fid);
    fprintf('Total sum: %d\n', total_sum);
end

main();
