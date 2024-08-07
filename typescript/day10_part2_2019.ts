interface Asteroid {
    x: number;
    y: number;
    angle: number;
    dist: number;
}

function main() {
    const asteroids = readAsteroids("input.txt");
    const station = findBestAsteroidLocation(asteroids);
    const vaporized = vaporizeAsteroids(asteroids, station);
    if (vaporized.length >= 200) {
        const result = vaporized[199].x * 100 + vaporized[199].y;
        console.log(result);
    } else {
        console.log("Less than 200 asteroids were vaporized.");
    }
}

function readAsteroids(filename: string): boolean[][] {
    const fs = require('fs');
    const lines = fs.readFileSync(filename, 'utf-8').split('\n');
    return lines.map((line: string) => Array.from(line).map((char: string) => char === '#'));
}

function vaporizeAsteroids(asteroids: boolean[][], station: [number, number]): Asteroid[] {
    const targets: Asteroid[] = [];
    for (let y = 0; y < asteroids.length; y++) {
        for (let x = 0; x < asteroids[y].length; x++) {
            if (asteroids[y][x] && !(x === station[0] && y === station[1])) {
                const angle = Math.atan2(y - station[1], x - station[0]);
                const dist = Math.hypot(x - station[0], y - station[1]);
                const adjustedAngle = angle < -Math.PI / 2 ? angle + 2 * Math.PI : angle;
                targets.push({ x, y, angle: adjustedAngle, dist });
            }
        }
    }

    targets.sort((a, b) => a.angle === b.angle ? a.dist - b.dist : a.angle - b.angle);

    const vaporized: Asteroid[] = [];
    while (targets.length > 0) {
        let lastAngle = -Infinity;
        for (let i = 0; i < targets.length;) {
            if (targets[i].angle !== lastAngle) {
                vaporized.push(targets[i]);
                lastAngle = targets[i].angle;
                targets.splice(i, 1);
            } else {
                i++;
            }
        }
    }
    return vaporized;
}

function findBestAsteroidLocation(asteroids: boolean[][]): [number, number] {
    let maxCount = 0;
    let bestLocation: [number, number] = [0, 0];
    for (let y = 0; y < asteroids.length; y++) {
        for (let x = 0; x < asteroids[y].length; x++) {
            if (asteroids[y][x]) {
                const count = countVisibleAsteroids(asteroids, x, y);
                if (count > maxCount) {
                    maxCount = count;
                    bestLocation = [x, y];
                }
            }
        }
    }
    return bestLocation;
}

function countVisibleAsteroids(asteroids: boolean[][], x: number, y: number): number {
    const angles = new Set<number>();
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

main();