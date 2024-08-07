import * as fs from 'fs';

interface Point {
    x: number;
    y: number;
}

const DIRECTIONS = [
    { x: 0, y: 1 },
    { x: 1, y: 0 },
    { x: 0, y: -1 },
    { x: -1, y: 0 }
];

function readInput(filename: string): string[] {
    return fs.readFileSync(filename, 'utf-8').split('\n').filter(line => line.length > 0);
}

function parseMap(input: string[]): [string[][], Map<number, Point>] {
    const map: string[][] = input.map(line => line.split(''));
    const locations = new Map<number, Point>();

    for (let y = 0; y < map.length; y++) {
        for (let x = 0; x < map[y].length; x++) {
            const cell = map[y][x];
            if (cell !== '#' && cell !== '.') {
                const num = parseInt(cell, 10);
                locations.set(num, { x, y });
            }
        }
    }

    return [map, locations];
}

function bfs(map: string[][], start: Point, end: Point): number {
    const queue: [Point, number][] = [[start, 0]];
    const visited = new Set<string>();
    visited.add(`${start.x},${start.y}`);

    while (queue.length > 0) {
        const [current, dist] = queue.shift()!;

        if (current.x === end.x && current.y === end.y) {
            return dist;
        }

        for (const dir of DIRECTIONS) {
            const next: Point = { x: current.x + dir.x, y: current.y + dir.y };
            const key = `${next.x},${next.y}`;

            if (map[next.y][next.x] !== '#' && !visited.has(key)) {
                visited.add(key);
                queue.push([next, dist + 1]);
            }
        }
    }

    return Infinity;
}

function findShortestPath(distances: Map<string, number>, start: number, locations: number[]): number {
    const n = locations.length;
    const allVisited = (1 << n) - 1;
    const memo = new Map<string, number>();

    function dfs(current: number, visited: number): number {
        if (visited === allVisited) {
            return distances.get(`${current}-${start}`)!;
        }

        const key = `${current}-${visited}`;
        if (memo.has(key)) {
            return memo.get(key)!;
        }

        let minDist = Infinity;

        for (let i = 0; i < n; i++) {
            if (!(visited & (1 << i))) {
                const next = locations[i];
                const newDist = distances.get(`${current}-${next}`)! + dfs(next, visited | (1 << i));
                minDist = Math.min(minDist, newDist);
            }
        }

        memo.set(key, minDist);
        return minDist;
    }

    return dfs(start, 1 << locations.indexOf(start));
}

function main() {
    const input = readInput('input.txt');
    const [map, locations] = parseMap(input);
    const locationNumbers = Array.from(locations.keys());
    const distances = new Map<string, number>();

    for (const startNum of locationNumbers) {
        for (const endNum of locationNumbers) {
            if (startNum !== endNum) {
                const start = locations.get(startNum)!;
                const end = locations.get(endNum)!;
                const dist = bfs(map, start, end);
                distances.set(`${startNum}-${endNum}`, dist);
            }
        }
    }

    const start = 0;
    const shortestPath = findShortestPath(distances, start, locationNumbers);
    console.log(`Part 1: ${shortestPath}`);

    const shortestPathReturning = findShortestPath(distances, start, locationNumbers.filter(num => num !== start));
    console.log(`Part 2: ${shortestPathReturning}`);
}

main();