
function main()
    fileID = fopen('input.txt', 'r');
    line = fgetl(fileID);
    fclose(fileID);

    if ischar(line)
        len = decompress(line, 1, length(line));
        fprintf('%d\n', len);
    end
end

function totalLength = decompress(input, startIdx, endIdx)
    totalLength = int64(0);
    i = startIdx;
    while i <= endIdx
        if input(i) == '('
            closeParenIdx = i + find(input(i:endIdx) == ')', 1) - 1;
            marker = sscanf(input(i+1:closeParenIdx-1), '%dx%d');
            charCount = marker(1);
            repeatCount = marker(2);
            
            nextIdx = closeParenIdx + 1;
            subLength = decompress(input, nextIdx, nextIdx + charCount - 1);
            totalLength = totalLength + int64(repeatCount) * subLength;
            
            i = nextIdx + charCount;
        else
            totalLength = totalLength + 1;
            i = i + 1;
        end
    end
end
