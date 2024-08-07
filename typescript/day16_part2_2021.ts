import * as fs from 'fs';

const hexToBinary = (hex: string): string => 
    hex.split('').map(h => parseInt(h, 16).toString(2).padStart(4, '0')).join('');

const parsePacket = (binary: string, offset: number): { versionSum: number, value: number, nextOffset: number } => {
    const version = parseInt(binary.slice(offset, offset + 3), 2);
    const typeId = parseInt(binary.slice(offset + 3, offset + 6), 2);
    offset += 6;
    let versionSum = version;
    let value = 0; // Initialize value to prevent usage before assignment

    if (typeId === 4) {
        let literal = '';
        while (true) {
            const group = binary.slice(offset, offset + 5);
            literal += group.slice(1);
            offset += 5;
            if (group[0] === '0') break;
        }
        value = parseInt(literal, 2);
    } else {
        const lengthTypeId = binary[offset++];
        let subPackets: number[] = [];

        if (lengthTypeId === '0') {
            const totalLength = parseInt(binary.slice(offset, offset + 15), 2);
            offset += 15;
            const endOffset = offset + totalLength;
            while (offset < endOffset) {
                const result = parsePacket(binary, offset);
                versionSum += result.versionSum;
                subPackets.push(result.value);
                offset = result.nextOffset;
            }
        } else {
            const numSubPackets = parseInt(binary.slice(offset, offset + 11), 2);
            offset += 11;
            for (let i = 0; i < numSubPackets; i++) {
                const result = parsePacket(binary, offset);
                versionSum += result.versionSum;
                subPackets.push(result.value);
                offset = result.nextOffset;
            }
        }

        switch (typeId) {
            case 0: value = subPackets.reduce((a, b) => a + b, 0); break; // sum
            case 1: value = subPackets.reduce((a, b) => a * b, 1); break; // product
            case 2: value = Math.min(...subPackets); break; // min
            case 3: value = Math.max(...subPackets); break; // max
            case 5: value = subPackets[0] > subPackets[1] ? 1 : 0; break; // greater than
            case 6: value = subPackets[0] < subPackets[1] ? 1 : 0; break; // less than
            case 7: value = subPackets[0] === subPackets[1] ? 1 : 0; break; // equal to
        }
    }

    return { versionSum, value, nextOffset: offset };
};

const main = () => {
    const hexInput = fs.readFileSync('input.txt', 'utf-8').trim();
    const binaryInput = hexToBinary(hexInput);
    const { versionSum, value } = parsePacket(binaryInput, 0);
    
    console.log(`Version Sum: ${versionSum}`);
    console.log(`Value of the outermost packet: ${value}`);
};

main();