import * as fs from 'fs';
import * as path from 'path';

type Point = { x: number, y: number };
type Map = string[][];

function readInput(filePath: string): string[] {
    const fileContent = fs.readFileSync(filePath, 'utf-8');
    return fileContent.split('\n').map(line => line.trim());
}

function parseMap(lines: string[]): { map: Map, points: { [key: string]: Point } } {
    const map: Map = lines.map(line => line.split(''));
    const points: { [key: string]: Point } = {};

    for (let y = 0; y < map.length; y++) {
        for (let x = 0; x < map[y].length; x++) {
            const cell = map[y][x];
            if (cell >= '0' && cell <= '9') {
                points[cell] = { x, y };
            }
        }
    }

    return { map, points };
}

function bfs(map: Map, start: Point, end: Point): number {
    const queue: [Point, number][] = [[start, 0]];
    const visited = new Set<string>();
    const directions = [
        { x: 1, y: 0 },
        { x: -1, y: 0 },
        { x: 0, y: 1 },
        { x: 0, y: -1 }
    ];

    while (queue.length > 0) {
        const [current, steps] = queue.shift()!;
        if (current.x === end.x && current.y === end.y) {
            return steps;
        }

        for (const dir of directions) {
            const next: Point = { x: current.x + dir.x, y: current.y + dir.y };
            const key = `${next.x},${next.y}`;
            if (visited.has(key) || map[next.y][next.x] === '#') {
                continue;
            }
            visited.add(key);
            queue.push([next, steps + 1]);
        }
    }

    return -1;
}

function findShortestPath(map: Map, points: { [key: string]: Point }): number {
    const pointKeys = Object.keys(points).filter(key => key !== '0');
    const n = pointKeys.length;
    const allPaths: number[][] = [];

    for (let i = 0; i < n; i++) {
        allPaths[i] = [];
        for (let j = 0; j < n; j++) {
            if (i !== j) {
                allPaths[i][j] = bfs(map, points[pointKeys[i]], points[pointKeys[j]]);
            }
        }
    }

    const startPoint = points['0'];
    const startPaths: number[] = [];
    for (let i = 0; i < n; i++) {
        startPaths[i] = bfs(map, startPoint, points[pointKeys[i]]);
    }

    let minPathLength = Infinity;
    const permute = (arr: number[]) => {
        if (arr.length === n) {
            let pathLength = startPaths[arr[0]];
            for (let i = 1; i < n; i++) {
                pathLength += allPaths[arr[i - 1]][arr[i]];
            }
            minPathLength = Math.min(minPathLength, pathLength);
            return;
        }

        for (let i = 0; i < n; i++) {
            if (!arr.includes(i)) {
                permute([...arr, i]);
            }
        }
    };

    permute([]);
    return minPathLength;
}

function main() {
    const filePath = path.join(__dirname, 'input.txt');
    const lines = readInput(filePath);
    const { map, points } = parseMap(lines);
    const shortestPath = findShortestPath(map, points);
    console.log(shortestPath);
}

main();