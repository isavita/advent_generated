import * as fs from 'fs';
import * as readline from 'readline';

interface Point {
    x: number;
    y: number;
    risk: number;
}

const readInput = async (filePath: string): Promise<number[][]> => {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const grid: number[][] = [];
    for await (const line of rl) {
        grid.push([...line].map(Number));
    }
    return grid;
};

const dijkstra = (grid: number[][]): number => {
    const rows = grid.length;
    const cols = grid[0].length;
    const riskLevels = Array.from({ length: rows }, () => Array(cols).fill(Infinity));
    riskLevels[0][0] = 0;

    const directions = [
        { x: 1, y: 0 },
        { x: 0, y: 1 },
        { x: -1, y: 0 },
        { x: 0, y: -1 }
    ];

    const priorityQueue: Point[] = [{ x: 0, y: 0, risk: 0 }];

    while (priorityQueue.length) {
        priorityQueue.sort((a, b) => a.risk - b.risk);
        const { x, y, risk } = priorityQueue.shift()!;

        if (x === rows - 1 && y === cols - 1) {
            return risk;
        }

        for (const { x: dx, y: dy } of directions) {
            const newX = x + dx;
            const newY = y + dy;

            if (newX >= 0 && newY >= 0 && newX < rows && newY < cols) {
                const newRisk = risk + grid[newX][newY];
                if (newRisk < riskLevels[newX][newY]) {
                    riskLevels[newX][newY] = newRisk;
                    priorityQueue.push({ x: newX, y: newY, risk: newRisk });
                }
            }
        }
    }

    return -1; // In case there's no path, though it shouldn't happen here.
};

const main = async () => {
    const grid = await readInput('input.txt');
    const result = dijkstra(grid);
    console.log(result);
};

main().catch(console.error);