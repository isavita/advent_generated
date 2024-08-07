import { readFileSync } from 'fs';

const input = readFileSync('input.txt', 'utf-8').trim();
const match = input.match(/x=(\d+)\.\.(\d+), y=(-?\d+)\.\.(-?\d+)/);
if (!match) throw new Error("Input format is incorrect");

const [xMin, xMax, yMin, yMax] = match.slice(1).map(Number);

let maxY = Number.NEGATIVE_INFINITY;
let validVelocities = new Set<string>();

for (let xVel = 1; xVel <= xMax; xVel++) {
    for (let yVel = yMin; yVel <= Math.abs(yMin); yVel++) {
        let x = 0, y = 0, currXVel = xVel, currYVel = yVel;
        let highestY = 0;

        while (x <= xMax && y >= yMin) {
            x += currXVel;
            y += currYVel;
            highestY = Math.max(highestY, y);

            if (x >= xMin && x <= xMax && y >= yMin && y <= yMax) {
                maxY = Math.max(maxY, highestY);
                validVelocities.add(`${xVel},${yVel}`);
                break;
            }

            currXVel = Math.max(0, currXVel - 1);
            currYVel -= 1;
        }
    }
}

console.log(`Highest Y position: ${maxY}`);
console.log(`Distinct initial velocities: ${validVelocities.size}`);