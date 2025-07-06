
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Unable to open input.txt');
    end
    inputLine = fgetl(fid);
    fclose(fid);

    decompressedLength = getDecompressedLength(inputLine);
    disp(decompressedLength);
end

function length = getDecompressedLength(input)
    length = 0;
    i = 1;
    while i <= numel(input)
        markerMatch = regexp(input(i:end), '\((\d+)x(\d+)\)', 'tokens', 'once');
        if ~isempty(markerMatch)
            charCount = str2double(markerMatch{1});
            repeatCount = str2double(markerMatch{2});
            length = length + charCount * repeatCount;
            i = i + numel(sprintf('(%dx%d)', charCount, repeatCount)) + charCount;
        else
            length = length + 1;
            i = i + 1;
        end
    end
end

main();
