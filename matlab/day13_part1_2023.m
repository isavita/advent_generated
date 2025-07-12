
function solve()
    mirrors = {};
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file');
    end

    currentMirrorLines = {};
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        if isempty(line)
            if ~isempty(currentMirrorLines)
                mirrors{end+1} = parseMirror(currentMirrorLines);
                currentMirrorLines = {};
            end
        else
            currentMirrorLines{end+1} = line;
        end
    end
    if ~isempty(currentMirrorLines)
        mirrors{end+1} = parseMirror(currentMirrorLines);
    end
    fclose(fid);

    totalReflection = 0;
    for i = 1:length(mirrors)
        mirror = mirrors{i};
        totalReflection = totalReflection + getMirrorAxis(mirror.cols, mirror.numCols);
        totalReflection = totalReflection + getMirrorAxis(mirror.rows, mirror.numRows) * 100;
    end
    fprintf('%d\n', totalReflection);
end

function mirror = parseMirror(mirrorStr)
    numLines = length(mirrorStr);
    numCols = length(mirrorStr{1});
    mirror.numRows = numLines;
    mirror.numCols = numCols;

    mirror.rows = zeros(1, numLines);
    for y = 1:numLines
        rowBinary = 0;
        for x = 1:numCols
            rowBinary = bitshift(rowBinary, 1);
            if mirrorStr{y}(x) == '#'
                rowBinary = bitor(rowBinary, 1);
            end
        end
        mirror.rows(y) = rowBinary;
    end

    mirror.cols = zeros(1, numCols);
    for x = 1:numCols
        colBinary = 0;
        for y = 1:numLines
            colBinary = bitshift(colBinary, 1);
            if mirrorStr{y}(x) == '#'
                colBinary = bitor(colBinary, 1);
            end
        end
        mirror.cols(x) = colBinary;
    end
end

function axis = getMirrorAxis(lines, numLines)
    for i = 1:(numLines - 1)
        isMirror = true;
        for j = 0:min(i - 1, numLines - i - 1)
            if lines(i - j) ~= lines(i + 1 + j)
                isMirror = false;
                break;
            end
        end
        if isMirror
            axis = i;
            return;
        end
    end
    axis = 0;
end

solve();
