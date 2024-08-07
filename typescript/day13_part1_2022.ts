import * as fs from 'fs';

const readInput = (filePath: string): string[] => {
    return fs.readFileSync(filePath, 'utf-8').trim().split('\n\n');
};

const parsePacket = (packet: string): any => JSON.parse(packet);

const comparePackets = (left: any, right: any): number => {
    if (Array.isArray(left) && Array.isArray(right)) {
        for (let i = 0; i < Math.max(left.length, right.length); i++) {
            if (i >= left.length) return -1; // left ran out of items
            if (i >= right.length) return 1; // right ran out of items
            const result = comparePackets(left[i], right[i]);
            if (result !== 0) return result;
        }
    } else if (Array.isArray(left)) {
        return comparePackets(left, [right]);
    } else if (Array.isArray(right)) {
        return comparePackets([left], right);
    } else {
        return left - right; // both are integers
    }
    return 0; // they are equal
};

const calculateSumOfIndices = (pairs: string[]): number => {
    let sum = 0;
    pairs.forEach((pair, index) => {
        const [left, right] = pair.split('\n').map(parsePacket);
        if (comparePackets(left, right) < 0) {
            sum += index + 1; // index is 0-based, so add 1
        }
    });
    return sum;
};

const main = () => {
    const input = readInput('input.txt');
    const result = calculateSumOfIndices(input);
    console.log(result);
};

main();