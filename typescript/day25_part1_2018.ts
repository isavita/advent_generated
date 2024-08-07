import * as fs from 'fs';

type Point = number[];

const readInput = (filename: string): Point[] => {
    const data = fs.readFileSync(filename, 'utf-8');
    return data.trim().split('\n').map(line => line.split(',').map(Number) as Point);
};

const manhattanDistance = (a: Point, b: Point): number => {
    return a.reduce((sum, val, index) => sum + Math.abs(val - b[index]), 0);
};

const findConstellations = (points: Point[]): number => {
    const visited = new Set<number>();
    let constellations = 0;

    const dfs = (index: number) => {
        visited.add(index);
        for (let i = 0; i < points.length; i++) {
            if (!visited.has(i) && manhattanDistance(points[index], points[i]) <= 3) {
                dfs(i);
            }
        }
    };

    for (let i = 0; i < points.length; i++) {
        if (!visited.has(i)) {
            dfs(i);
            constellations++;
        }
    }

    return constellations;
};

const main = () => {
    const points = readInput('input.txt');
    const result = findConstellations(points);
    console.log(result);
};

main();