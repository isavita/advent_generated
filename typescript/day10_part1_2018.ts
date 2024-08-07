import * as fs from 'fs';

type Point = {
    x: number;
    y: number;
    vx: number;
    vy: number;
};

const readInput = (filename: string): Point[] => {
    const data = fs.readFileSync(filename, 'utf8');
    const regex = /position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/;
    return data.split('\n').map(line => {
        const match = line.match(regex);
        return {
            x: parseInt(match![1]),
            y: parseInt(match![2]),
            vx: parseInt(match![3]),
            vy: parseInt(match![4])
        };
    }).filter(point => point !== undefined);
};

const simulate = (points: Point[], seconds: number): Point[] => {
    for (let t = 0; t < seconds; t++) {
        points.forEach(point => {
            point.x += point.vx;
            point.y += point.vy;
        });
    }
    return points;
};

const printMessage = (points: Point[]) => {
    const minX = Math.min(...points.map(p => p.x));
    const minY = Math.min(...points.map(p => p.y));
    const maxX = Math.max(...points.map(p => p.x));
    const maxY = Math.max(...points.map(p => p.y));

    const grid: string[][] = Array.from({ length: maxY - minY + 1 }, () => Array(maxX - minX + 1).fill('.'));

    points.forEach(point => {
        grid[point.y - minY][point.x - minX] = '#';
    });

    console.log(grid.map(row => row.join('')).join('\n'));
};

const main = () => {
    const points = readInput('input.txt');
    let seconds = 0;
    let previousArea = Infinity;

    while (true) {
        const currentPoints = simulate(points.map(p => ({ ...p })), seconds);
        const minX = Math.min(...currentPoints.map(p => p.x));
        const minY = Math.min(...currentPoints.map(p => p.y));
        const maxX = Math.max(...currentPoints.map(p => p.x));
        const maxY = Math.max(...currentPoints.map(p => p.y));
        const area = (maxX - minX) * (maxY - minY);

        if (area > previousArea) break;
        previousArea = area;
        seconds++;
    }

    const finalPoints = simulate(points.map(p => ({ ...p })), seconds - 1);
    printMessage(finalPoints);
};

main();