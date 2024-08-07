import * as fs from 'fs';

type Position = [number, number];
type State = {
    pos: Position;
    keys: Set<string>;
    steps: number;
};

const directions: Position[] = [[0, 1], [1, 0], [0, -1], [-1, 0]];

function readInput(filePath: string): string[][] {
    return fs.readFileSync(filePath, 'utf-8').trim().split('\n').map(line => line.split(''));
}

function isKey(cell: string): boolean {
    return cell >= 'a' && cell <= 'z';
}

function isDoor(cell: string): boolean {
    return cell >= 'A' && cell <= 'Z';
}

function bfs(maze: string[][], start: Position): number {
    const rows = maze.length;
    const cols = maze[0].length;
    const totalKeys = [...new Set(maze.flat().filter(isKey))].length;
    const queue: State[] = [{ pos: start, keys: new Set(), steps: 0 }];
    const visited = new Set<string>();

    while (queue.length) {
        const { pos, keys, steps } = queue.shift()!;
        const keyString = `${pos[0]},${pos[1]},${[...keys].sort().join('')}`;

        if (visited.has(keyString)) continue;
        visited.add(keyString);

        if (keys.size === totalKeys) return steps;

        for (const [dx, dy] of directions) {
            const newPos: Position = [pos[0] + dx, pos[1] + dy];
            const cell = maze[newPos[0]]?.[newPos[1]];

            if (!cell || cell === '#') continue;

            const newKeys = new Set(keys);
            if (isKey(cell)) newKeys.add(cell);
            if (isDoor(cell) && !keys.has(cell.toLowerCase())) continue;

            queue.push({ pos: newPos, keys: newKeys, steps: steps + 1 });
        }
    }
    return -1; // If no solution is found
}

function findStart(maze: string[][]): Position {
    for (let i = 0; i < maze.length; i++) {
        for (let j = 0; j < maze[i].length; j++) {
            if (maze[i][j] === '@') return [i, j];
        }
    }
    throw new Error('Start position not found');
}

function main() {
    const maze = readInput('input.txt');
    const startPosition = findStart(maze);
    const result = bfs(maze, startPosition);
    console.log(result);
}

main();