const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n')[0].split(', ');
const xRange = data[0].slice(15).split('..');
const yRange = data[1].slice(2).split('..');
const xMin = parseInt(xRange[0]);
const xMax = parseInt(xRange[1]);
const yMin = parseInt(yRange[0]);
const yMax = parseInt(yRange[1]);

const velocities = new Set();
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

            if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
                inTargetArea = true;
                break;
            }

            if (isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax)) {
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
            velocities.add(`${xVel},${yVel}`);
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