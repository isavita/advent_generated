const fs = require('fs');

const content = fs.readFileSync('input.txt', 'utf8');
const coordinates = parseCoordinates(content);
const regionSize = findRegionSize(coordinates, 10000);

console.log(regionSize);

function parseCoordinates(input) {
    const lines = input.trim().split('\n');
    const coordinates = [];

    for (const line of lines) {
        const [x, y] = line.split(',').map(Number);
        coordinates.push({ x, y });
    }

    return coordinates;
}

function findRegionSize(coordinates, maxDistance) {
    const [minX, minY, maxX, maxY] = findBoundingBox(coordinates);
    let regionSize = 0;

    for (let x = minX; x <= maxX; x++) {
        for (let y = minY; y <= maxY; y++) {
            let totalDistance = 0;

            for (const c of coordinates) {
                totalDistance += manhattanDistance(x, y, c.x, c.y);
            }

            if (totalDistance < maxDistance) {
                regionSize++;
            }
        }
    }

    return regionSize;
}

function findBoundingBox(coordinates) {
    let minX = Infinity, minY = Infinity;
    let maxX = -Infinity, maxY = -Infinity;

    for (const c of coordinates) {
        minX = Math.min(minX, c.x);
        minY = Math.min(minY, c.y);
        maxX = Math.max(maxX, c.x);
        maxY = Math.max(maxY, c.y);
    }

    return [minX, minY, maxX, maxY];
}

function manhattanDistance(x1, y1, x2, y2) {
    return Math.abs(x1 - x2) + Math.abs(y1 - y2);
}