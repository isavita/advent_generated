import * as fs from 'fs';
import * as readline from 'readline';

interface Sue {
    number: number;
    properties: Map<string, number>;
}

const MFCSAMOutput: Map<string, number> = new Map([
    ['children', 3],
    ['cats', 7],
    ['samoyeds', 2],
    ['pomeranians', 3],
    ['akitas', 0],
    ['vizslas', 0],
    ['goldfish', 5],
    ['trees', 3],
    ['cars', 2],
    ['perfumes', 1]
]);

const ranges: Map<string, (a: number, b: number) => boolean> = new Map([
    ['cats', (a, b) => a > b],
    ['trees', (a, b) => a > b],
    ['pomeranians', (a, b) => a < b],
    ['goldfish', (a, b) => a < b]
]);

async function processLineByLine() {
    const fileStream = fs.createReadStream('input.txt');

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const sues: Sue[] = [];

    for await (const line of rl) {
        const sue = parseSue(line);
        sues.push(sue);
    }

    const part1Sue = findSue(sues, false);
    console.log(`Part 1: ${part1Sue}`);

    const part2Sue = findSue(sues, true);
    console.log(`Part 2: ${part2Sue}`);
}

function parseSue(line: string): Sue {
    const sueMatch = line.match(/Sue (\d+): (.+)/);
    if (!sueMatch) throw new Error('Invalid input format');

    const number = parseInt(sueMatch[1], 10);
    const properties = new Map<string, number>();

    sueMatch[2].split(', ').forEach(property => {
        const [key, value] = property.split(': ');
        properties.set(key, parseInt(value, 10));
    });

    return { number, properties };
}

function findSue(sues: Sue[], useRanges: boolean): number {
    for (const sue of sues) {
        let match = true;
        for (const [key, value] of sue.properties) {
            if (!MFCSAMOutput.has(key)) continue;

            const mfcsamValue = MFCSAMOutput.get(key)!;
            if (useRanges && ranges.has(key)) {
                const compareFn = ranges.get(key)!;
                if (!compareFn(value, mfcsamValue)) {
                    match = false;
                    break;
                }
            } else {
                if (value !== mfcsamValue) {
                    match = false;
                    break;
                }
            }
        }
        if (match) return sue.number;
    }
    throw new Error('No matching Sue found');
}

processLineByLine().catch(console.error);