import * as fs from 'fs';

function calculatePowerConsumption(filename: string): number {
    const data = fs.readFileSync(filename, 'utf-8').trim().split('\n');
    const bitLength = data[0].length;
    const counts = Array(bitLength).fill(0);

    data.forEach(line => {
        for (let i = 0; i < bitLength; i++) {
            counts[i] += line[i] === '1' ? 1 : -1;
        }
    });

    const gammaRate = parseInt(counts.map(count => (count > 0 ? '1' : '0')).join(''), 2);
    const epsilonRate = parseInt(counts.map(count => (count < 0 ? '1' : '0')).join(''), 2);

    return gammaRate * epsilonRate;
}

console.log(calculatePowerConsumption('input.txt'));