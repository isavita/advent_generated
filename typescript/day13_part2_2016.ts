import * as fs from 'fs';

// Function to read input from file
function readInput(filePath: string): number {
    const data = fs.readFileSync(filePath, 'utf8');
    return parseInt(data.trim(), 10);
}

// Function to determine if a location is a wall or open space
function isWall(x: number, y: number, favoriteNumber: number): boolean {
    const value = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
    const binary = value.toString(2);
    const numOfOnes = binary.split('').reduce((count, bit) => count + (bit === '1' ? 1 : 0), 0);
    return numOfOnes % 2 === 1;
}

// Function to find the shortest path using BFS
function shortestPath(start: [number, number], target: [number, number], favoriteNumber: number): number {
    const queue: [number, number, number][] = [[start[0], start[1], 0]];
    const visited = new Set<string>();
    visited.add(`${start[0]},${start[1]}`);

    while (queue.length > 0) {
        const [x, y, steps] = queue.shift()!;

        if (x === target[0] && y === target[1]) {
            return steps;
        }

        const directions = [
            [0, 1],
            [0, -1],
            [1, 0],
            [-1, 0]
        ];

        for (const [dx, dy] of directions) {
            const nx = x + dx;
            const ny = y + dy;
            const key = `${nx},${ny}`;

            if (nx >= 0 && ny >= 0 && !visited.has(key) && !isWall(nx, ny, favoriteNumber)) {
                visited.add(key);
                queue.push([nx, ny, steps + 1]);
            }
        }
    }

    return -1; // Should not reach here
}

// Function to find the number of locations reachable within a certain number of steps
function countReachableLocations(start: [number, number], maxSteps: number, favoriteNumber: number): number {
    const queue: [number, number, number][] = [[start[0], start[1], 0]];
    const visited = new Set<string>();
    visited.add(`${start[0]},${start[1]}`);

    let count = 0;

    while (queue.length > 0) {
        const [x, y, steps] = queue.shift()!;

        if (steps <= maxSteps) {
            count++;
        } else {
            continue;
        }

        const directions = [
            [0, 1],
            [0, -1],
            [1, 0],
            [-1, 0]
        ];

        for (const [dx, dy] of directions) {
            const nx = x + dx;
            const ny = y + dy;
            const key = `${nx},${ny}`;

            if (nx >= 0 && ny >= 0 && !visited.has(key) && !isWall(nx, ny, favoriteNumber)) {
                visited.add(key);
                queue.push([nx, ny, steps + 1]);
            }
        }
    }

    return count;
}

// Main function
function main() {
    const favoriteNumber = readInput('input.txt');
    const start: [number, number] = [1, 1];
    const target: [number, number] = [31, 39];
    const maxSteps = 50;

    const fewestSteps = shortestPath(start, target, favoriteNumber);
    const reachableLocations = countReachableLocations(start, maxSteps, favoriteNumber);

    console.log(`Fewest number of steps required to reach 31,39: ${fewestSteps}`);
    console.log(`Number of locations reachable in at most 50 steps: ${reachableLocations}`);
}

main();