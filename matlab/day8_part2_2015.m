
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file: input.txt');
    end
    
    totalDiff = 0;
    
    line = fgetl(fid);
    while ischar(line)
        originalLength = numel(line);
        encodedLength = calculateEncodedLength(line);
        totalDiff = totalDiff + (encodedLength - originalLength);
        line = fgetl(fid);
    end
    
    fclose(fid);
    
    fprintf('%d\n', totalDiff);
end

function encodedLength = calculateEncodedLength(s)
    encodedLength = 2; 
    for i = 1:numel(s)
        ch = s(i);
        if ch == '\' || ch == '"'
            encodedLength = encodedLength + 1;
        end
        encodedLength = encodedLength + 1;
    end
end

main();
