
function main()
    fid = fopen('input.txt', 'r');
    hexInput = fscanf(fid, '%s');
    fclose(fid);

    binaryInput = hexToBinary(hexInput);
    [versionSum, value] = parsePacket(binaryInput, 1);

    fprintf('Version Sum: %d\n', versionSum);
    fprintf('Value of the outermost packet: %d\n', value);
end

function binary = hexToBinary(hex)
    binary = '';
    for i = 1:length(hex)
        h = hex(i);
        val = hex2dec(h);
        binVal = dec2bin(val, 4);
        binary = [binary, binVal];
    end
end

function [versionSum, value, nextOffset] = parsePacket(binary, offset)
    version = bin2dec(binary(offset : offset + 2));
    typeId = bin2dec(binary(offset + 3 : offset + 5));
    offset = offset + 6;
    versionSum = version;
    value = 0;

    if typeId == 4
        literal = '';
        while true
            group = binary(offset : offset + 4);
            literal = [literal, group(2:5)];
            offset = offset + 5;
            if group(1) == '0'
                break;
            end
        end
        value = bin2dec(literal);
    else
        lengthTypeId = binary(offset);
        offset = offset + 1;
        subPackets = [];

        if lengthTypeId == '0'
            totalLength = bin2dec(binary(offset : offset + 14));
            offset = offset + 15;
            endOffset = offset + totalLength;
            while offset < endOffset
                [vSum, val, nextOff] = parsePacket(binary, offset);
                versionSum = versionSum + vSum;
                subPackets = [subPackets, val];
                offset = nextOff;
            end
        else
            numSubPackets = bin2dec(binary(offset : offset + 10));
            offset = offset + 11;
            for i = 1:numSubPackets
                [vSum, val, nextOff] = parsePacket(binary, offset);
                versionSum = versionSum + vSum;
                subPackets = [subPackets, val];
                offset = nextOff;
            end
        end

        switch typeId
            case 0
                value = sum(subPackets);
            case 1
                value = prod(subPackets);
            case 2
                value = min(subPackets);
            case 3
                value = max(subPackets);
            case 5
                value = subPackets(1) > subPackets(2);
            case 6
                value = subPackets(1) < subPackets(2);
            case 7
                value = subPackets(1) == subPackets(2);
        end
    end
    nextOffset = offset;
end

main();
