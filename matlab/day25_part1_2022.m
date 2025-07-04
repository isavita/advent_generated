
function main()
    fid = fopen('input.txt', 'r');
    sum = 0;
    while ~feof(fid)
        line = strtrim(fgets(fid));
        sum = sum + from_snafu(line);
    end
    fclose(fid);
    disp(to_snafu(sum));
end

function n = from_snafu(s)
    n = 0;
    for i = 1:length(s)
        char = s(i);
        n = n * 5;
        if char == '='
            n = n - 2;
        elseif char == '-'
            n = n - 1;
        else
            n = n + str2double(char);
        end
    end
end

function snafu_str = to_snafu(n)
    if n == 0
        snafu_str = '0';
        return;
    end
    b = '';
    while n > 0
        remainder = mod(n, 5);
        if remainder == 3
            n = n + 5;
            b = ['=', b];
        elseif remainder == 4
            n = n + 5;
            b = ['-', b];
        else
            b = [num2str(remainder), b];
        end
        n = floor(n / 5);
    end
    snafu_str = b;
end

main();
