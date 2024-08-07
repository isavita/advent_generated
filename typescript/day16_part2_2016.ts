import * as fs from 'fs';

function generateDragonCurve(a: string): string {
    let b = a.split('').reverse().map(bit => bit === '0' ? '1' : '0').join('');
    return a + '0' + b;
}

function generateData(initialState: string, length: number): string {
    let data = initialState;
    while (data.length < length) {
        data = generateDragonCurve(data);
    }
    return data.slice(0, length);
}

function calculateChecksum(data: string): string {
    let checksum = data;
    while (checksum.length % 2 === 0) {
        let nextChecksum = [];
        for (let i = 0; i < checksum.length; i += 2) {
            nextChecksum.push(checksum[i] === checksum[i + 1] ? '1' : '0');
        }
        checksum = nextChecksum.join('');
    }
    return checksum;
}

function main() {
    const filePath = 'input.txt';
    const initialState = fs.readFileSync(filePath, 'utf-8').trim();
    const diskLength = 35651584; // Change to 272 for Part One

    const data = generateData(initialState, diskLength);
    const checksum = calculateChecksum(data);

    console.log(checksum);
}

main();