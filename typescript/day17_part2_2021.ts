
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(', ');
const xRange = input[0].slice(15).split('..').map(Number);
const yRange = input[1].slice(2).split('..').map(Number);

const velocities = new Map();

for (let xVel = -1000; xVel <= 1000; xVel++) {
    for (let yVel = -1000; yVel <= 1000; yVel++) {
        let xPos = 0;
        let yPos = 0;
        let curXVel = xVel;
        let curYVel = yVel;
        let inTargetArea = false;

        while (true) {
            xPos += curXVel;
            yPos += curYVel;

            if (xPos >= xRange[0] && xPos <= xRange[1] && yPos >= yRange[0] && yPos <= yRange[1]) {
                inTargetArea = true;
                break;
            }

            if (isMovingAway(xPos, yPos, curXVel, curYVel, xRange[0], xRange[1], yRange[0], yRange[1])) {
                break;
            }

            if (curXVel > 0) {
                curXVel--;
            } else if (curXVel < 0) {
                curXVel++;
            }

            curYVel--;
        }

        if (inTargetArea) {
            velocities.set(`${xVel},${yVel}`, true);
        }
    }
}

console.log(velocities.size);

function isMovingAway(xPos, yPos, xVel, yVel, xMin, xMax, yMin, yMax) {
    if (xPos < xMin && xVel < 0) {
        return true;
    }
    if (xPos > xMax && xVel > 0) {
        return true;
    }
    if (yPos < yMin && yVel < 0) {
        return true;
    }
    return false;
}
