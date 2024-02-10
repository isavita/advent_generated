const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const heightmap = input.map(row => row.split('').map(Number));

const basinSizes = [];
const visited = new Map();

function isLowPoint(heightmap, x, y) {
    const height = heightmap[y][x];
    if (x > 0 && heightmap[y][x - 1] <= height) {
        return false;
    }
    if (x < heightmap[y].length - 1 && heightmap[y][x + 1] <= height) {
        return false;
    }
    if (y > 0 && heightmap[y - 1][x] <= height) {
        return false;
    }
    if (y < heightmap.length - 1 && heightmap[y + 1][x] <= height) {
        return false;
    }
    return true;
}

function exploreBasin(heightmap, x, y, visited) {
    if (visited.has(`${x},${y}`) || heightmap[y][x] === 9) {
        return 0;
    }
    visited.set(`${x},${y}`, true);
    let size = 1;

    const directions = [[0, -1], [-1, 0], [0, 1], [1, 0]];
    for (const dir of directions) {
        const newX = x + dir[0];
        const newY = y + dir[1];
        if (newX >= 0 && newX < heightmap[0].length && newY >= 0 && newY < heightmap.length) {
            size += exploreBasin(heightmap, newX, newY, visited);
        }
    }
    return size;
}

heightmap.forEach((row, y) => {
    row.forEach((_, x) => {
        if (isLowPoint(heightmap, x, y)) {
            const size = exploreBasin(heightmap, x, y, visited);
            basinSizes.push(size);
        }
    });
});

basinSizes.sort((a, b) => b - a);
const result = basinSizes[0] * basinSizes[1] * basinSizes[2];
console.log(result);