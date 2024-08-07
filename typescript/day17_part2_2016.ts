import * as fs from 'fs';
import * as crypto from 'crypto';

// Function to calculate MD5 hash
function md5(input: string): string {
    return crypto.createHash('md5').update(input).digest('hex');
}

// Function to check if a door is open
function isOpen(door: string): boolean {
    return 'bcdef'.includes(door);
}

// Function to get the status of doors
function getDoorStatus(passcode: string, path: string): [boolean, boolean, boolean, boolean] {
    const hash = md5(passcode + path);
    return [
        isOpen(hash[0]),
        isOpen(hash[1]),
        isOpen(hash[2]),
        isOpen(hash[3])
    ];
}

// BFS to find the shortest path
function shortestPath(passcode: string): string {
    const queue: [number, number, string][] = [[0, 0, '']];
    const visited = new Set<string>();

    while (queue.length > 0) {
        const [x, y, path] = queue.shift()!;

        if (x === 3 && y === 3) {
            return path;
        }

        const [up, down, left, right] = getDoorStatus(passcode, path);

        if (up && y > 0 && !visited.has(`${x},${y - 1}`)) {
            queue.push([x, y - 1, path + 'U']);
            visited.add(`${x},${y - 1}`);
        }
        if (down && y < 3 && !visited.has(`${x},${y + 1}`)) {
            queue.push([x, y + 1, path + 'D']);
            visited.add(`${x},${y + 1}`);
        }
        if (left && x > 0 && !visited.has(`${x - 1},${y}`)) {
            queue.push([x - 1, y, path + 'L']);
            visited.add(`${x - 1},${y}`);
        }
        if (right && x < 3 && !visited.has(`${x + 1},${y}`)) {
            queue.push([x + 1, y, path + 'R']);
            visited.add(`${x + 1},${y}`);
        }
    }

    return '';
}

// DFS to find the longest path
function longestPath(passcode: string): number {
    const queue: [number, number, string][] = [[0, 0, '']];
    let maxLength = 0;

    while (queue.length > 0) {
        const [x, y, path] = queue.pop()!;

        if (x === 3 && y === 3) {
            maxLength = Math.max(maxLength, path.length);
            continue;
        }

        const [up, down, left, right] = getDoorStatus(passcode, path);

        if (up && y > 0) {
            queue.push([x, y - 1, path + 'U']);
        }
        if (down && y < 3) {
            queue.push([x, y + 1, path + 'D']);
        }
        if (left && x > 0) {
            queue.push([x - 1, y, path + 'L']);
        }
        if (right && x < 3) {
            queue.push([x + 1, y, path + 'R']);
        }
    }

    return maxLength;
}

// Read input from file
const passcode = fs.readFileSync('input.txt', 'utf8').trim();

// Find the shortest path
const shortest = shortestPath(passcode);
console.log(`Shortest path: ${shortest}`);

// Find the longest path
const longest = longestPath(passcode);
console.log(`Longest path length: ${longest}`);