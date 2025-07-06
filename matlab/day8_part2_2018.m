
function main()
    data = readInput('input.txt');
    value = parseTree(data, 1);
    disp(value);
end

function data = readInput(filename)
    fid = fopen(filename, 'r');
    line = fgetl(fid);
    fclose(fid);
    data = str2num(line);
end

function [value, index] = parseTree(data, index)
    childCount = data(index);
    metaCount = data(index + 1);
    index = index + 2;

    childValues = zeros(1, childCount);
    for i = 1:childCount
        [childValues(i), index] = parseTree(data, index);
    end

    value = 0;
    if childCount == 0
        value = sum(data(index : index + metaCount - 1));
    else
        for i = 1:metaCount
            metadata = data(index + i - 1);
            if metadata <= childCount && metadata > 0
                value = value + childValues(metadata);
            end
        end
    end
    index = index + metaCount;
end
