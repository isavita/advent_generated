
function main
    fid = fopen('input.txt', 'r');
    sum = 0;
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        if isempty(line)
            continue;
        end
        digits = regexp(line, '\d', 'match');
        if ~isempty(digits)
            first_digit = str2double(digits{1});
            last_digit = str2double(digits{end});
            value = str2double([num2str(first_digit), num2str(last_digit)]);
            sum = sum + value;
        end
    end
    fclose(fid);
    disp(sum);
end

main;
