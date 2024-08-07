import * as fs from 'fs';

interface Nanobot {
    position: [number, number, number];
    radius: number;
}

const parseInput = (data: string): Nanobot[] => {
    return data.trim().split('\n').map(line => {
        const match = line.match(/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/);
        return match ? {
            position: [parseInt(match[1]), parseInt(match[2]), parseInt(match[3])],
            radius: parseInt(match[4])
        } : null;
    }).filter(Boolean) as Nanobot[];
};

const manhattanDistance = (a: [number, number, number], b: [number, number, number]): number => {
    return Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) + Math.abs(a[2] - b[2]);
};

const countInRange = (nanobots: Nanobot[], strongest: Nanobot): number => {
    return nanobots.filter(nanobot => manhattanDistance(nanobot.position, strongest.position) <= strongest.radius).length;
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const nanobots = parseInput(data);
    const strongest = nanobots.reduce((prev, curr) => (curr.radius > prev.radius ? curr : prev));
    const inRangeCount = countInRange(nanobots, strongest);
    console.log(inRangeCount);
};

main();