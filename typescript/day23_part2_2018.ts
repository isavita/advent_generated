
import * as fs from 'fs';
import * as path from 'path';

type Nanobot = { x: number; y: number; z: number; r: number };
type Cube = { x: number; y: number; z: number; size: number };

function parseInput(filePath: string): Nanobot[] {
    const fileContent = fs.readFileSync(path.resolve(__dirname, filePath), 'utf-8');
    const nanobots: Nanobot[] = [];
    const pattern = /pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/;
    for (const line of fileContent.split('\n')) {
        const match = line.trim().match(pattern);
        if (match) {
            const [, xStr, yStr, zStr, rStr] = match;
            const x = parseInt(xStr, 10);
            const y = parseInt(yStr, 10);
            const z = parseInt(zStr, 10);
            const r = parseInt(rStr, 10);
            nanobots.push({ x, y, z, r });
        }
    }
    return nanobots;
}

function manhattanDistance(a: { x: number; y: number; z: number }, b: { x: number; y: number; z: number }): number {
    return Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z);
}

function partOne(nanobots: Nanobot[]): number {
    const strongest = nanobots.reduce((maxBot, bot) => (bot.r > maxBot.r ? bot : maxBot), nanobots[0]);
    let count = 0;
    for (const bot of nanobots) {
        if (manhattanDistance(strongest, bot) <= strongest.r) {
            count++;
        }
    }
    return count;
}

function minDistanceToOrigin(cube: Cube): number {
    let dx = 0;
    if (cube.x > 0) {
        dx = cube.x;
    } else if (cube.x + cube.size - 1 < 0) {
        dx = -(cube.x + cube.size - 1);
    }

    let dy = 0;
    if (cube.y > 0) {
        dy = cube.y;
    } else if (cube.y + cube.size - 1 < 0) {
        dy = -(cube.y + cube.size - 1);
    }

    let dz = 0;
    if (cube.z > 0) {
        dz = cube.z;
    } else if (cube.z + cube.size - 1 < 0) {
        dz = -(cube.z + cube.size - 1);
    }

    return dx + dy + dz;
}


function countNanobotsInRange(cube: Cube, nanobots: Nanobot[]): number {
    let count = 0;
    for (const bot of nanobots) {
        let d = 0;
        if (bot.x < cube.x) {
            d += cube.x - bot.x;
        } else if (bot.x > cube.x + cube.size - 1) {
            d += bot.x - (cube.x + cube.size - 1);
        }
        if (bot.y < cube.y) {
            d += cube.y - bot.y;
        } else if (bot.y > cube.y + cube.size - 1) {
            d += bot.y - (cube.y + cube.size - 1);
        }
        if (bot.z < cube.z) {
            d += cube.z - bot.z;
        } else if (bot.z > cube.z + cube.size - 1) {
            d += bot.z - (cube.z + cube.size - 1);
        }
        if (d <= bot.r) {
            count++;
        }
    }
    return count;
}


function partTwo(nanobots: Nanobot[]): number {
    let minX = nanobots.reduce((min, bot) => Math.min(min, bot.x), nanobots[0].x);
    let maxX = nanobots.reduce((max, bot) => Math.max(max, bot.x), nanobots[0].x);
    let minY = nanobots.reduce((min, bot) => Math.min(min, bot.y), nanobots[0].y);
    let maxY = nanobots.reduce((max, bot) => Math.max(max, bot.y), nanobots[0].y);
    let minZ = nanobots.reduce((min, bot) => Math.min(min, bot.z), nanobots[0].z);
    let maxZ = nanobots.reduce((max, bot) => Math.max(max, bot.z), nanobots[0].z);

    let size = 1;
    while (size < Math.max(maxX - minX, maxY - minY, maxZ - minZ)) {
        size *= 2;
    }

    const priorityQueue: [number, number, Cube][] = [];  // [-count, distance, cube]

    priorityQueue.push([-countNanobotsInRange({ x: minX, y: minY, z: minZ, size }, nanobots), minDistanceToOrigin({ x: minX, y: minY, z: minZ, size }), { x: minX, y: minY, z: minZ, size }]);
    priorityQueue.sort((a, b) => a[0] - b[0] || a[1] - b[1]);


    let bestDistance: number | null = null;
    let bestCount = -1;

    while (priorityQueue.length > 0) {
        const [negCount, distance, cube] = priorityQueue.shift()!;
        const count = -negCount;

        if (cube.size === 1) {
            if (count > bestCount || (count === bestCount && distance < (bestDistance === null ? Infinity : bestDistance))) {
                bestCount = count;
                bestDistance = distance;
                break;
            }
            continue;
        }

        const half = cube.size / 2;
        for (const dx of [0, half]) {
            for (const dy of [0, half]) {
                for (const dz of [0, half]) {
                    const nx = cube.x + dx;
                    const ny = cube.y + dy;
                    const nz = cube.z + dz;
                    const newSize = half;

                    const newCube: Cube = { x: nx, y: ny, z: nz, size: newSize };

                    const newCount = countNanobotsInRange(newCube, nanobots);
                    const newDistance = minDistanceToOrigin(newCube);

                    priorityQueue.push([-newCount, newDistance, newCube]);
                    priorityQueue.sort((a, b) => a[0] - b[0] || a[1] - b[1]); //Sort descending by count, then ascending by distance.

                }
            }
        }
    }

    return bestDistance!;
}


function main(): void {
    const inputFilePath = 'input.txt';
    const nanobots = parseInput(inputFilePath);

    const countInRange = partOne(nanobots);
    console.log(`Part One: ${countInRange}`);

    const shortestDistance = partTwo(nanobots);
    console.log(`Part Two: ${shortestDistance}`);
}

main();
