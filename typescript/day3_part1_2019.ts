import * as fs from 'fs';

function parseInput(filePath: string): string[][] {
    const data = fs.readFileSync(filePath, 'utf-8').trim().split('\n');
    return data.map(line => line.split(','));
}

function getWirePath(wire: string[]): Set<string> {
    const path = new Set<string>();
    let x = 0, y = 0;

    for (const move of wire) {
        const direction = move[0];
        const distance = parseInt(move.slice(1), 10);
        
        for (let i = 0; i < distance; i++) {
            if (direction === 'R') x++;
            else if (direction === 'L') x--;
            else if (direction === 'U') y++;
            else if (direction === 'D') y--;
            path.add(`${x},${y}`);
        }
    }

    return path;
}

function getClosestIntersection(wire1: string[], wire2: string[]): number {
    const path1 = getWirePath(wire1);
    const path2 = getWirePath(wire2);
    
    const intersections = [...path1].filter(point => path2.has(point));
    const distances = intersections.map(point => {
        const [x, y] = point.split(',').map(Number);
        return Math.abs(x) + Math.abs(y);
    });

    return Math.min(...distances);
}

const wires = parseInput('input.txt');
const closestDistance = getClosestIntersection(wires[0], wires[1]);
console.log(closestDistance);