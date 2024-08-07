import * as fs from 'fs';

const hexToBinary = (hex: string): string => {
    return hex.split('').map(char => parseInt(char, 16).toString(2).padStart(4, '0')).join('');
};

const parsePacket = (binary: string, index: { value: number }): number => {
    const version = parseInt(binary.slice(index.value, index.value + 3), 2);
    index.value += 3;
    const typeId = parseInt(binary.slice(index.value, index.value + 3), 2);
    index.value += 3;

    let versionSum = version;

    if (typeId === 4) {
        // Literal value packet
        let literalValue = '';
        let more = true;

        while (more) {
            more = binary[index.value] === '1';
            literalValue += binary.slice(index.value + 1, index.value + 5);
            index.value += 5;
        }
        // Convert binary literal to decimal (not used for version sum)
    } else {
        // Operator packet
        const lengthTypeId = binary[index.value];
        index.value += 1;

        if (lengthTypeId === '0') {
            const totalLength = parseInt(binary.slice(index.value, index.value + 15), 2);
            index.value += 15;
            const endIndex = index.value + totalLength;

            while (index.value < endIndex) {
                versionSum += parsePacket(binary, index);
            }
        } else {
            const numberOfSubPackets = parseInt(binary.slice(index.value, index.value + 11), 2);
            index.value += 11;

            for (let i = 0; i < numberOfSubPackets; i++) {
                versionSum += parsePacket(binary, index);
            }
        }
    }

    return versionSum;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const binary = hexToBinary(input);
    const versionSum = parsePacket(binary, { value: 0 });
    console.log(versionSum);
};

main();