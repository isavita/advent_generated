import * as fs from 'fs';
import * as path from 'path';

const inputFilePath = path.join(__dirname, 'input.txt');

function readInput(filePath: string): number[][] {
    const data = fs.readFileSync(filePath, 'utf-8').trim().split('\n');
    return data.map(line => line.split('').map(Number));
}

function expandMap(original: number[][]): number[][] {
    const height = original.length;
    const width = original[0].length;
    const expanded: number[][] = Array.from({ length: height * 5 }, () => Array(width * 5).fill(0));

    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            for (let dy = 0; dy < 5; dy++) {
                for (let dx = 0; dx < 5; dx++) {
                    const risk = (original[y][x] + dy + dx - 1) % 9 + 1;
                    expanded[y + dy * height][x + dx * width] = risk;
                }
            }
        }
    }
    return expanded;
}

function dijkstra(map: number[][]): number {
    const height = map.length;
    const width = map[0].length;
    const dist = Array.from({ length: height }, () => Array(width).fill(Infinity));
    const pq: [number, number, number][] = []; // [risk, y, x]

    dist[0][0] = 0;
    pq.push([0, 0, 0]);

    const directions = [[1, 0], [0, 1], [-1, 0], [0, -1]];

    while (pq.length) {
        pq.sort((a, b) => a[0] - b[0]);
        const [currentRisk, y, x] = pq.shift()!;

        if (y === height - 1 && x === width - 1) return currentRisk;

        for (const [dy, dx] of directions) {
            const newY = y + dy, newX = x + dx;
            if (newY >= 0 && newY < height && newX >= 0 && newX < width) {
                const newRisk = currentRisk + map[newY][newX];
                if (newRisk < dist[newY][newX]) {
                    dist[newY][newX] = newRisk;
                    pq.push([newRisk, newY, newX]);
                }
            }
        }
    }
    return dist[height - 1][width - 1];
}

const originalMap = readInput(inputFilePath);
const expandedMap = expandMap(originalMap);
const lowestRisk = dijkstra(expandedMap);

console.log(lowestRisk);