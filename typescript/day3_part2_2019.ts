import * as fs from 'fs';

type Point = [number, number];

const parseInput = (filePath: string): string[][] => {
    const data = fs.readFileSync(filePath, 'utf-8').trim().split('\n');
    return data.map(line => line.split(','));
};

const getPath = (wire: string[]): Set<Point> => {
    const path = new Set<Point>();
    let [x, y] = [0, 0];
    let steps = 0;

    for (const move of wire) {
        const direction = move[0];
        const distance = parseInt(move.slice(1), 10);

        for (let i = 0; i < distance; i++) {
            if (direction === 'R') x++;
            else if (direction === 'L') x--;
            else if (direction === 'U') y++;
            else if (direction === 'D') y--;

            path.add([x, y]);
        }
    }
    return path;
};

const getIntersections = (path1: Set<Point>, path2: Set<Point>): Point[] => {
    return [...path1].filter(point => path2.has(point));
};

const manhattanDistance = (point: Point): number => {
    return Math.abs(point[0]) + Math.abs(point[1]);
};

const getFewestSteps = (wire1: string[], wire2: string[]): number => {
    const path1 = new Map<string, number>();
    let [x, y] = [0, 0];
    let steps = 0;

    for (const move of wire1) {
        const direction = move[0];
        const distance = parseInt(move.slice(1), 10);

        for (let i = 0; i < distance; i++) {
            if (direction === 'R') x++;
            else if (direction === 'L') x--;
            else if (direction === 'U') y++;
            else if (direction === 'D') y--;

            steps++;
            path1.set(`${x},${y}`, steps);
        }
    }

    [x, y] = [0, 0];
    steps = 0;
    let minSteps = Infinity;

    for (const move of wire2) {
        const direction = move[0];
        const distance = parseInt(move.slice(1), 10);

        for (let i = 0; i < distance; i++) {
            if (direction === 'R') x++;
            else if (direction === 'L') x--;
            else if (direction === 'U') y++;
            else if (direction === 'D') y--;

            steps++;
            const key = `${x},${y}`;
            if (path1.has(key)) {
                minSteps = Math.min(minSteps, steps + path1.get(key)!);
            }
        }
    }

    return minSteps;
};

const main = () => {
    const [wire1, wire2] = parseInput('input.txt');
    const path1 = getPath(wire1);
    const path2 = getPath(wire2);

    const intersections = getIntersections(path1, path2);
    const closestDistance = Math.min(...intersections.map(manhattanDistance));
    const fewestSteps = getFewestSteps(wire1, wire2);

    console.log(`Closest intersection distance: ${closestDistance}`);
    console.log(`Fewest combined steps: ${fewestSteps}`);
};

main();