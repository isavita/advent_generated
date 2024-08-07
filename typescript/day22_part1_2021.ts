import * as fs from 'fs';

type Command = 'on' | 'off';
interface Step {
    command: Command;
    xRange: [number, number];
    yRange: [number, number];
    zRange: [number, number];
}

const parseInput = (data: string): Step[] => {
    return data.trim().split('\n').map(line => {
        const [command, ranges] = line.split(' ');
        const [xRange, yRange, zRange] = ranges.split(',').map(range => {
            const [start, end] = range.split('=')[1].split('..').map(Number) as [number, number];
            return [start, end] as [number, number];
        });
        return { command: command as Command, xRange, yRange, zRange };
    });
};

const executeSteps = (steps: Step[]): number => {
    const cubes: Set<string> = new Set();

    for (const { command, xRange, yRange, zRange } of steps) {
        const [xStart, xEnd] = xRange;
        const [yStart, yEnd] = yRange;
        const [zStart, zEnd] = zRange;

        for (let x = Math.max(xStart, -50); x <= Math.min(xEnd, 50); x++) {
            for (let y = Math.max(yStart, -50); y <= Math.min(yEnd, 50); y++) {
                for (let z = Math.max(zStart, -50); z <= Math.min(zEnd, 50); z++) {
                    const key = `${x},${y},${z}`;
                    if (command === 'on') {
                        cubes.add(key);
                    } else {
                        cubes.delete(key);
                    }
                }
            }
        }
    }

    return cubes.size;
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const steps = parseInput(data);
    const result = executeSteps(steps);
    console.log(result);
};

main();