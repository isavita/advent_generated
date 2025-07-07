
function main()
    fid = fopen('input.txt', 'r');
    data = fscanf(fid, '%d');
    fclose(fid);
    
    [~, sumVal] = sumMetadata(data);
    disp(sumVal);
end

function [nextIndex, totalSum] = sumMetadata(data, index)
    if nargin < 2
        index = 1;
    end
    
    childCount = data(index);
    index = index + 1;
    metadataCount = data(index);
    index = index + 1;
    
    totalSum = 0;
    
    for i = 1:childCount
        [newIndex, childSum] = sumMetadata(data, index);
        totalSum = totalSum + childSum;
        index = newIndex;
    end
    
    for i = 1:metadataCount
        totalSum = totalSum + data(index);
        index = index + 1;
    end
    
    nextIndex = index;
end

main();
