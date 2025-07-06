
function main()
    fid = fopen('input.txt', 'r');
    components = textscan(fid, '%d/%d');
    fclose(fid);
    
    components = [components{1}, components{2}];
    
    maxStrength = 0;
    maxLength = 0;
    
    [maxStrength, maxLength] = findStrongestLongestBridge(components, zeros(size(components, 1), 1), 0, 0, 0, maxStrength, maxLength);
    
    fprintf('%d\n', maxStrength);
end

function [maxStrength, maxLength] = findStrongestLongestBridge(components, used, port, strength, length, currentMaxStrength, currentMaxLength)
    if length > currentMaxLength || (length == currentMaxLength && strength > currentMaxStrength)
        maxStrength = strength;
        maxLength = length;
    else
        maxStrength = currentMaxStrength;
        maxLength = currentMaxLength;
    end

    for i = 1:size(components, 1)
        if used(i)
            continue;
        end
        
        if components(i, 1) == port || components(i, 2) == port
            used(i) = 1;
            nextPort = components(i, 1);
            if components(i, 1) == port
                nextPort = components(i, 2);
            end
            
            [newMaxStrength, newMaxLength] = findStrongestLongestBridge(components, used, nextPort, strength + components(i, 1) + components(i, 2), length + 1, maxStrength, maxLength);
            
            maxStrength = newMaxStrength;
            maxLength = newMaxLength;
            
            used(i) = 0;
        end
    end
end

main();
