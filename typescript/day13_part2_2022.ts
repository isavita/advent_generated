import * as fs from 'fs';

type Packet = number | Packet[];

const readInput = (filename: string): Packet[][] => {
    const content = fs.readFileSync(filename, 'utf-8');
    return content.trim().split('\n\n').map(pair => 
        pair.split('\n').map(packet => JSON.parse(packet))
    );
};

const comparePackets = (left: Packet, right: Packet): number => {
    if (typeof left === 'number' && typeof right === 'number') {
        return left - right;
    }
    if (typeof left === 'number') {
        return comparePackets([left], right);
    }
    if (typeof right === 'number') {
        return comparePackets(left, [right]);
    }
    
    for (let i = 0; i < Math.min(left.length, right.length); i++) {
        const cmp = comparePackets(left[i], right[i]);
        if (cmp !== 0) return cmp;
    }
    return left.length - right.length;
};

const partOne = (pairs: Packet[][]): number => {
    return pairs.reduce((sum, [left, right], index) => {
        return sum + (comparePackets(left, right) < 0 ? index + 1 : 0);
    }, 0);
};

const partTwo = (pairs: Packet[][]): number => {
    const packets = pairs.flat().concat([[[2]], [[6]]]);
    packets.sort(comparePackets);
    const idx2 = packets.findIndex(packet => JSON.stringify(packet) === '[[2]]') + 1;
    const idx6 = packets.findIndex(packet => JSON.stringify(packet) === '[[6]]') + 1;
    return idx2 * idx6;
};

const main = () => {
    const pairs = readInput('input.txt');
    console.log('Part One:', partOne(pairs));
    console.log('Part Two:', partTwo(pairs));
};

main();