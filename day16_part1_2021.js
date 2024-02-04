const fs = require('fs');

function hexToBin(hex) {
    let bin = "";
    for (let i = 0; i < hex.length; i++) {
        let b = parseInt(hex[i], 16);
        bin += b.toString(2).padStart(4, '0');
    }
    return bin;
}

function parsePacket(binStr, idx) {
    let version = (parseInt(binStr[idx]) << 2) | (parseInt(binStr[idx + 1]) << 1) | parseInt(binStr[idx + 2]);
    let typeID = (parseInt(binStr[idx + 3]) << 2) | (parseInt(binStr[idx + 4]) << 1) | parseInt(binStr[idx + 5]);
    idx += 6;

    if (typeID === 4) {
        while (binStr[idx] === '1') {
            idx += 5;
        }
        idx += 5;
        return [version, idx];
    }

    let lengthTypeID = parseInt(binStr[idx]);
    idx++;
    let numSubPackets, subPacketLength;

    if (lengthTypeID === 0) {
        subPacketLength = 0;
        for (let i = 0; i < 15; i++) {
            subPacketLength = (subPacketLength << 1) | parseInt(binStr[idx]);
            idx++;
        }
    } else {
        numSubPackets = 0;
        for (let i = 0; i < 11; i++) {
            numSubPackets = (numSubPackets << 1) | parseInt(binStr[idx]);
            idx++;
        }
    }

    let versionSum = version;
    while (true) {
        if ((lengthTypeID === 0 && subPacketLength === 0) || (lengthTypeID === 1 && numSubPackets === 0)) {
            break;
        }
        let [subVersion, newIndex] = parsePacket(binStr, idx);
        versionSum += subVersion;

        if (lengthTypeID === 0) {
            subPacketLength -= newIndex - idx;
        } else {
            numSubPackets--;
        }
        idx = newIndex;
    }
    return [versionSum, idx];
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error reading file:", err);
        process.exit(1);
    }

    let hexStr = data.trim();
    let binStr = hexToBin(hexStr);
    let [versionSum, _] = parsePacket(binStr, 0);
    console.log(versionSum);
});