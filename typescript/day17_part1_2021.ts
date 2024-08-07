import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const targetArea = parseTargetArea(input);

function parseTargetArea(input: string): { xRange: [number, number], yRange: [number, number] } {
    const match = input.match(/x=(\d+)\.\.(\d+), y=([-]?\d+)\.\.([-]?\d+)/);
    if (!match) throw new Error('Invalid input format');
    return {
        xRange: [parseInt(match[1]), parseInt(match[2])],
        yRange: [parseInt(match[3]), parseInt(match[4])]
    };
}

function isWithinTargetArea(x: number, y: number, target: { xRange: [number, number], yRange: [number, number] }): boolean {
    return x >= target.xRange[0] && x <= target.xRange[1] && y >= target.yRange[0] && y <= target.yRange[1];
}

function simulateProbe(vx: number, vy: number, target: { xRange: [number, number], yRange: [number, number] }): number | null {
    let x = 0, y = 0;
    let maxY = 0;

    while (x <= target.xRange[1] && y >= target.yRange[0]) {
        x += vx;
        y += vy;
        maxY = Math.max(maxY, y);

        if (isWithinTargetArea(x, y, target)) {
            return maxY;
        }

        vx = Math.max(0, vx - 1);
        vy -= 1;
    }
    return null;
}

function findHighestY(target: { xRange: [number, number], yRange: [number, number] }): number {
    let highestY = 0;

    for (let vx = 1; vx <= target.xRange[1]; vx++) {
        for (let vy = target.yRange[0]; vy <= Math.abs(target.yRange[0]); vy++) {
            const maxY = simulateProbe(vx, vy, target);
            if (maxY !== null) {
                highestY = Math.max(highestY, maxY);
            }
        }
    }

    return highestY;
}

const highestYPosition = findHighestY(targetArea);
console.log(highestYPosition);