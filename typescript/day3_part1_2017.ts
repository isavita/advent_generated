import { readFileSync } from 'fs';

const calculateSteps = (input: number): number => {
    if (input === 1) return 0;

    let layer = Math.ceil((Math.sqrt(input) - 1) / 2);
    let maxInLayer = (2 * layer + 1) ** 2;
    let sideLength = 2 * layer;
    let offset = (maxInLayer - input) % sideLength;

    let distanceToCenter = Math.abs(offset - layer);
    return layer + distanceToCenter;
};

const main = () => {
    const data = readFileSync('input.txt', 'utf-8').trim();
    const input = parseInt(data, 10);
    const steps = calculateSteps(input);
    console.log(steps);
};

main();