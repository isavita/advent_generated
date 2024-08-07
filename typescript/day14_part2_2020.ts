import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

type Memory = { [key: string]: number };

const applyMaskToAddress = (address: number, mask: string): string => {
    const binaryAddress = address.toString(2).padStart(36, '0').split('');
    for (let i = 0; i < 36; i++) {
        if (mask[i] === '1') binaryAddress[i] = '1';
        else if (mask[i] === 'X') binaryAddress[i] = 'X';
    }
    return binaryAddress.join('');
};

const generateAddresses = (address: string): number[] => {
    const addresses: string[] = [];
    const floatingCount = address.split('X').length - 1;
    const combinations = 1 << floatingCount;

    for (let i = 0; i < combinations; i++) {
        const currentAddress = address.split('');
        let bitIndex = 0;

        for (let j = 0; j < currentAddress.length; j++) {
            if (currentAddress[j] === 'X') {
                currentAddress[j] = (i & (1 << bitIndex)) ? '1' : '0';
                bitIndex++;
            }
        }
        addresses.push(currentAddress.join(''));
    }

    return addresses.map(addr => parseInt(addr, 2));
};

const memory: Memory = {};
let currentMask = '';

for (const line of input) {
    if (line.startsWith('mask')) {
        currentMask = line.split(' = ')[1];
    } else {
        const [_, addrStr, valueStr] = line.match(/mem\[(\d+)\] = (\d+)/) || [];
        const address = parseInt(addrStr, 10);
        const value = parseInt(valueStr, 10);

        const maskedAddress = applyMaskToAddress(address, currentMask);
        const addresses = generateAddresses(maskedAddress);

        for (const addr of addresses) {
            memory[addr.toString()] = value;
        }
    }
}

const sum = Object.values(memory).reduce((acc, val) => acc + val, 0);
console.log(sum);