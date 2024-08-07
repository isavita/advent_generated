import * as fs from 'fs';
import * as path from 'path';

function readInput(filePath: string): number[] {
    const fileContent = fs.readFileSync(filePath, 'utf-8');
    return fileContent.split(/\s+/).map(Number);
}

function redistribute(banks: number[]): number[] {
    const maxBlocks = Math.max(...banks);
    const maxIndex = banks.indexOf(maxBlocks);
    const blocksToRedistribute = banks[maxIndex];

    banks[maxIndex] = 0;
    for (let i = 1; i <= blocksToRedistribute; i++) {
        banks[(maxIndex + i) % banks.length]++;
    }

    return banks;
}

function findRedistributionCycles(banks: number[]): number {
    const seenConfigurations = new Set<string>();
    let cycles = 0;
    let currentConfig = banks.join(',');

    while (!seenConfigurations.has(currentConfig)) {
        seenConfigurations.add(currentConfig);
        banks = redistribute(banks);
        currentConfig = banks.join(',');
        cycles++;
    }

    return cycles;
}

function main() {
    const filePath = path.join(__dirname, 'input.txt');
    const initialBanks = readInput(filePath);
    const cycles = findRedistributionCycles(initialBanks);
    console.log(cycles);
}

main();