import * as fs from 'fs';
import * as readline from 'readline';

async function main() {
    const fileStream = fs.createReadStream('input.txt');
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const grid: string[][] = [];
    for await (const line of rl) {
        grid.push(line.split(''));
    }

    let x = 0, y = 0;
    for (let i = 0; i < grid[0].length; i++) {
        if (grid[0][i] === '|') {
            x = i;
            break;
        }
    }

    let dx = 0, dy = 1;
    let steps = 0;

    while (true) {
        if (x < 0 || x >= grid[0].length || y < 0 || y >= grid.length) {
            break;
        }

        const cell = grid[y][x];

        if (cell === ' ') {
            break;
        }

        steps++;

        if (cell === '+') {
            if (dx === 0) {
                if (x > 0 && (grid[y][x - 1] === '-' || (grid[y][x - 1] >= 'A' && grid[y][x - 1] <= 'Z'))) {
                    dx = -1;
                    dy = 0;
                } else {
                    dx = 1;
                    dy = 0;
                }
            } else {
                if (y > 0 && (grid[y - 1][x] === '|' || (grid[y - 1][x] >= 'A' && grid[y - 1][x] <= 'Z'))) {
                    dx = 0;
                    dy = -1;
                } else {
                    dx = 0;
                    dy = 1;
                }
            }
        }

        x += dx;
        y += dy;
    }

    console.log(steps);
}

main().catch(console.error);