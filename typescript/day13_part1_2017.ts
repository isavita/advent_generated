import * as fs from 'fs';
import * as readline from 'readline';

interface Layer {
    depth: number;
    range: number;
}

async function readInput(filePath: string): Promise<Layer[]> {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const layers: Layer[] = [];

    for await (const line of rl) {
        const [depth, range] = line.split(': ').map(Number);
        layers.push({ depth, range });
    }

    return layers;
}

function calculateSeverity(layers: Layer[]): number {
    let severity = 0;

    for (const layer of layers) {
        if (layer.depth % (2 * (layer.range - 1)) === 0) {
            severity += layer.depth * layer.range;
        }
    }

    return severity;
}

async function main() {
    try {
        const layers = await readInput('input.txt');
        const severity = calculateSeverity(layers);
        console.log(`The severity of the trip is: ${severity}`);
    } catch (error) {
        console.error('Error reading input file:', error);
    }
}

main();