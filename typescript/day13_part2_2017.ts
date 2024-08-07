import * as fs from 'fs';

interface Layer {
    depth: number;
    range: number;
}

function parseInput(input: string): Layer[] {
    return input.split('\n')
        .filter(line => line.length > 0)
        .map(line => {
            const [depth, range] = line.split(': ').map(Number);
            return { depth, range };
        });
}

function calculateSeverity(layers: Layer[]): number {
    return layers.reduce((severity, layer) => {
        if (layer.depth % ((layer.range - 1) * 2) === 0) {
            return severity + layer.depth * layer.range;
        }
        return severity;
    }, 0);
}

function findMinimumDelay(layers: Layer[]): number {
    let delay = 0;
    while (true) {
        if (!isCaught(layers, delay)) {
            return delay;
        }
        delay++;
    }
}

function isCaught(layers: Layer[], delay: number): boolean {
    return layers.some(layer => {
        const position = (layer.depth + delay) % ((layer.range - 1) * 2);
        return position === 0;
    });
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const layers = parseInput(input);

    const severity = calculateSeverity(layers);
    console.log(`Trip Severity: ${severity}`);

    const minimumDelay = findMinimumDelay(layers);
    console.log(`Minimum Delay to Avoid Detection: ${minimumDelay}`);
}

main();