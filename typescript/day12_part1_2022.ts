import * as fs from 'fs';

interface Point {
    x: number;
    y: number;
}

const directions = [
    { x: 0, y: 1 },  // right
    { x: 1, y: 0 },  // down
    { x: 0, y: -1 }, // left
    { x: -1, y: 0 }  // up
];

const isValidMove = (current: string, next: string): boolean => {
    return next.charCodeAt(0) <= current.charCodeAt(0) + 1;
};

const bfs = (grid: string[][], start: Point, end: Point): number => {
    const queue: Point[] = [start];
    const visited = new Set<string>();
    visited.add(`${start.x},${start.y}`);
    let steps = 0;

    while (queue.length > 0) {
        const size = queue.length;
        for (let i = 0; i < size; i++) {
            const { x, y } = queue.shift()!;
            if (x === end.x && y === end.y) return steps;

            for (const { x: dx, y: dy } of directions) {
                const nx = x + dx;
                const ny = y + dy;
                if (nx >= 0 && ny >= 0 && nx < grid.length && ny < grid[0].length) {
                    const currentChar = grid[x][y];
                    const nextChar = grid[nx][ny];
                    if (isValidMove(currentChar, nextChar) && !visited.has(`${nx},${ny}`)) {
                        visited.add(`${nx},${ny}`);
                        queue.push({ x: nx, y: ny });
                    }
                }
            }
        }
        steps++;
    }
    return -1; // If no path is found
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const grid = input.map(line => line.split(''));
    let start: Point | null = null;
    let end: Point | null = null;

    for (let i = 0; i < grid.length; i++) {
        for (let j = 0; j < grid[i].length; j++) {
            if (grid[i][j] === 'S') {
                start = { x: i, y: j };
                grid[i][j] = 'a'; // Treat 'S' as elevation 'a'
            } else if (grid[i][j] === 'E') {
                end = { x: i, y: j };
                grid[i][j] = 'z'; // Treat 'E' as elevation 'z'
            }
        }
    }

    if (start && end) {
        const result = bfs(grid, start, end);
        console.log(result);
    } else {
        console.error('Start or end position not found.');
    }
};

main();