import * as fs from 'fs';
import * as readline from 'readline';

async function readInput(filePath: string): Promise<number[]> {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const input: number[] = [];
    for await (const line of rl) {
        input.push(...line.split(/\s+/).map(Number));
    }

    return input;
}

function reallocate(banks: number[]): number[] {
    const maxBlocks = Math.max(...banks);
    const maxIndex = banks.indexOf(maxBlocks);

    const newBanks = [...banks];
    newBanks[maxIndex] = 0;

    for (let i = 1; i <= maxBlocks; i++) {
        newBanks[(maxIndex + i) % newBanks.length]++;
    }

    return newBanks;
}

function findCycleLength(banks: number[]): [number, number] {
    const seenConfigurations = new Map<string, number>();
    let cycles = 0;
    let loopSize = 0;

    while (true) {
        const configKey = banks.join(',');
        if (seenConfigurations.has(configKey)) {
            loopSize = cycles - seenConfigurations.get(configKey)!;
            break;
        }
        seenConfigurations.set(configKey, cycles);
        banks = reallocate(banks);
        cycles++;
    }

    return [cycles, loopSize];
}

async function main() {
    const banks = await readInput('input.txt');
    const [cycles, loopSize] = findCycleLength(banks);

    console.log(`Number of redistributions before a configuration is seen again: ${cycles}`);
    console.log(`Size of the loop: ${loopSize}`);
}

main().catch(console.error);