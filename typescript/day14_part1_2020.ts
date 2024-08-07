import * as fs from 'fs';

type Memory = { [key: number]: number };

const applyMask = (value: number, mask: string): number => {
    const binaryValue = value.toString(2).padStart(36, '0').split('');
    for (let i = 0; i < mask.length; i++) {
        if (mask[i] !== 'X') {
            binaryValue[i] = mask[i];
        }
    }
    return parseInt(binaryValue.join(''), 2);
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const memory: Memory = {};
    let currentMask = '';

    for (const line of input) {
        if (line.startsWith('mask')) {
            currentMask = line.split(' = ')[1];
        } else {
            const [addressPart, valuePart] = line.split(' = ');
            const address = parseInt(addressPart.match(/\d+/)![0]);
            const value = parseInt(valuePart.trim());
            memory[address] = applyMask(value, currentMask);
        }
    }

    const sum = Object.values(memory).reduce((acc, val) => acc + val, 0);
    console.log(sum);
};

main();