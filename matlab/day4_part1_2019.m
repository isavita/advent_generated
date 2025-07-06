
function main()
    fid = fopen('input.txt', 'r');
    rangeStr = fgetl(fid);
    fclose(fid);

    parts = strsplit(rangeStr, '-');
    start = str2double(parts{1});
    ending = str2double(parts{2});

    count = 0;
    for i = start:ending
        s = num2str(i);
        if hasDoubleAndIncreasingDigits(s)
            count = count + 1;
        end
    end
    disp(count);
end

function result = hasDoubleAndIncreasingDigits(s)
    hasDouble = false;
    for i = 1:length(s)-1
        if s(i) == s(i+1)
            hasDouble = true;
        end
        if s(i) > s(i+1)
            result = false;
            return;
        end
    end
    result = hasDouble;
end

main();
