
import * as fs from 'fs';

const dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]];

function solve() {
    const input = fs.readFileSync('input.txt', 'utf8');
    const grid = input.split('\n').map(line => line.split('') as string[]);
    const h = grid.length;
    const w = grid[0].length;

    let x = 0, y = 0, dirIdx = 0;
    let foundStart = false;

    for (let i = 0; i < h && !foundStart; i++) {
        for (let j = 0; j < w && !foundStart; j++) {
            switch (grid[i][j]) {
                case '^': x = j; y = i; dirIdx = 0; foundStart = true; break;
                case '>': x = j; y = i; dirIdx = 1; foundStart = true; break;
                case 'v': x = j; y = i; dirIdx = 2; foundStart = true; break;
                case '<': x = j; y = i; dirIdx = 3; foundStart = true; break;
            }
        }
    }

    const visited = new Set<string>();
    visited.add(`${x},${y}`);

    while (true) {
        const nx = x + dirs[dirIdx][0];
        const ny = y + dirs[dirIdx][1];

        if (nx < 0 || nx >= w || ny < 0 || ny >= h) break;

        if (grid[ny][nx] === '#') {
            dirIdx = (dirIdx + 1) % 4;
            continue;
        }

        x = nx;
        y = ny;
        visited.add(`${x},${y}`);
    }

    console.log(visited.size);
}


solve();
