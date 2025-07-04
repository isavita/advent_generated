
function main()
    fid = fopen('input.txt', 'r');
    hexStr = fscanf(fid, '%s');
    fclose(fid);

    binStr = hexToBin(hexStr);
    [~, versionSum] = parsePacket(binStr, 1);
    disp(versionSum);
end

function binStr = hexToBin(hexStr)
    binStr = '';
    for i = 1:length(hexStr)
        hexChar = hexStr(i);
        decVal = hex2dec(hexChar);
        binVal = dec2bin(decVal, 4);
        binStr = [binStr, binVal];
    end
end

function [typeID, versionSum, nextIdx] = parsePacket(binStr, idx)
    version = bin2dec(binStr(idx:idx+2));
    typeID = bin2dec(binStr(idx+3:idx+5));
    idx = idx + 6;

    if typeID == 4
        while binStr(idx) == '1'
            idx = idx + 5;
        end
        idx = idx + 5;
        versionSum = version;
        nextIdx = idx;
        return;
    end

    lengthTypeID = str2double(binStr(idx));
    idx = idx + 1;

    if lengthTypeID == 0
        subPacketLength = bin2dec(binStr(idx:idx+14));
        idx = idx + 15;
        startIdx = idx;
        versionSum = version;
        while idx < startIdx + subPacketLength
            [~, subVersionSum, newIdx] = parsePacket(binStr, idx);
            versionSum = versionSum + subVersionSum;
            idx = newIdx;
        end
    else
        numSubPackets = bin2dec(binStr(idx:idx+10));
        idx = idx + 11;
        versionSum = version;
        for i = 1:numSubPackets
            [~, subVersionSum, newIdx] = parsePacket(binStr, idx);
            versionSum = versionSum + subVersionSum;
            idx = newIdx;
        end
    end
    nextIdx = idx;
end

main();
