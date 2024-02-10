const fs = require('fs');

function readAsteroids(filename) {
    const file = fs.readFileSync(filename, 'utf8');
    const lines = file.split('\n');
    const asteroids = [];
    for (let line of lines) {
        const asteroidRow = [];
        for (let char of line) {
            asteroidRow.push(char === '#');
        }
        asteroids.push(asteroidRow);
    }
    return asteroids;
}

function findBestAsteroidLocation(asteroids) {
    let maxCount = 0;
    for (let y = 0; y < asteroids.length; y++) {
        for (let x = 0; x < asteroids[y].length; x++) {
            if (asteroids[y][x]) {
                const count = countVisibleAsteroids(asteroids, x, y);
                if (count > maxCount) {
                    maxCount = count;
                }
            }
        }
    }
    return maxCount;
}

function countVisibleAsteroids(asteroids, x, y) {
    const angles = new Set();
    for (let otherY = 0; otherY < asteroids.length; otherY++) {
        for (let otherX = 0; otherX < asteroids[otherY].length; otherX++) {
            if (asteroids[otherY][otherX] && !(otherX === x && otherY === y)) {
                const angle = Math.atan2(otherY - y, otherX - x);
                angles.add(angle);
            }
        }
    }
    return angles.size;
}

const asteroids = readAsteroids("input.txt");
const maxCount = findBestAsteroidLocation(asteroids);
console.log(maxCount);