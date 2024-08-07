import * as fs from 'fs';

interface Point {
    x: number;
    y: number;
}

const readInput = (filename: string): string[] => {
    return fs.readFileSync(filename, 'utf-8').trim().split('\n');
};

const parseLine = (line: string): [Point, Point] => {
    const [start, end] = line.split(' -> ').map(coord => {
        const [x, y] = coord.split(',').map(Number);
        return { x, y };
    });
    return [start, end];
};

const getPoints = (start: Point, end: Point): Point[] => {
    const points: Point[] = [];
    const dx = Math.sign(end.x - start.x);
    const dy = Math.sign(end.y - start.y);
    let x = start.x;
    let y = start.y;

    while (x !== end.x || y !== end.y) {
        points.push({ x, y });
        if (x !== end.x) x += dx;
        if (y !== end.y) y += dy;
    }
    points.push(end); // Include the end point
    return points;
};

const countOverlaps = (lines: string[]): number => {
    const grid: Map<string, number> = new Map();
    for (const line of lines) {
        const [start, end] = parseLine(line);
        const points = getPoints(start, end);
        points.forEach(point => {
            const key = `${point.x},${point.y}`;
            grid.set(key, (grid.get(key) || 0) + 1);
        });
    }
    return Array.from(grid.values()).filter(count => count > 1).length;
};

const main = () => {
    const lines = readInput('input.txt');
    const overlapCount = countOverlaps(lines);
    console.log(overlapCount);
};

main();