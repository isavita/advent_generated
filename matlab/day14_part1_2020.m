
function main()
    fid = fopen('input.txt', 'r');
    
    mem = containers.Map('KeyType', 'uint64', 'ValueType', 'uint64');
    orMask = uint64(0);
    andMask = uint64(0);
    
    line = fgetl(fid);
    while ischar(line)
        if startsWith(line, 'mask')
            maskStr = line(8:end);
            orMask = uint64(bin2dec(strrep(maskStr, 'X', '0')));
            andMask = uint64(bin2dec(strrep(maskStr, 'X', '1')));
        else
            data = sscanf(line, 'mem[%d] = %d');
            address = uint64(data(1));
            value = uint64(data(2));
            
            maskedValue = bitor(bitand(value, andMask), orMask);
            mem(address) = maskedValue;
        end
        line = fgetl(fid);
    end
    
    fclose(fid);
    
    totalSum = sum(cell2mat(values(mem)));
    fprintf('%lu\n', totalSum);
end
