
function main()
    file = fopen("input.txt", "r");
    lengthsStr = strsplit(strtrim(fgets(file)), ",");
    fclose(file);
    lengths = cellfun(@str2double, lengthsStr);

    list = 0:255;
    currentPosition = 0;
    skipSize = 0;

    for length = lengths
        for i = 0:(floor(length/2) - 1)
            startIdx = mod(currentPosition + i, 256) + 1;
            endIdx = mod(currentPosition + length - 1 - i, 256) + 1;
            temp = list(startIdx);
            list(startIdx) = list(endIdx);
            list(endIdx) = temp;
        end
        currentPosition = mod(currentPosition + length + skipSize, 256);
        skipSize = skipSize + 1;
    end

    result = list(1) * list(2);
    disp(result);
end
