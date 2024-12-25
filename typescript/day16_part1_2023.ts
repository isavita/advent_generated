
import * as fs from 'fs';

type Direction = 'up' | 'down' | 'left' | 'right';
type Point = { row: number; col: number };
type Beam = { pos: Point; dir: Direction };

function solve(grid: string[][]): number {
    const rows = grid.length;
    const cols = grid[0].length;

    const startBeam: Beam = { pos: { row: 0, col: 0 }, dir: 'right' };
    const energized = new Set<string>();
    const visited = new Set<string>();
    const beams: Beam[] = [startBeam];

    while (beams.length > 0) {
        const currentBeam = beams.shift()!;
        const { pos, dir } = currentBeam;
        const key = `${pos.row},${pos.col},${dir}`;

        if (pos.row < 0 || pos.row >= rows || pos.col < 0 || pos.col >= cols || visited.has(key)) {
            continue;
        }

        visited.add(key);
        energized.add(`${pos.row},${pos.col}`);

        const tile = grid[pos.row][pos.col];

        let nextBeams: Beam[] = [];

        switch (tile) {
            case '.':
                nextBeams.push(moveBeam(pos, dir));
                break;
            case '/':
                nextBeams.push(moveBeam(pos, reflect(dir, '/')));
                break;
            case '\\':
                nextBeams.push(moveBeam(pos, reflect(dir, '\\')));
                break;
            case '|':
                if (dir === 'left' || dir === 'right') {
                    nextBeams.push(moveBeam(pos, 'up'));
                    nextBeams.push(moveBeam(pos, 'down'));
                } else {
                    nextBeams.push(moveBeam(pos, dir));
                }
                break;
            case '-':
                if (dir === 'up' || dir === 'down') {
                    nextBeams.push(moveBeam(pos, 'left'));
                    nextBeams.push(moveBeam(pos, 'right'));
                } else {
                    nextBeams.push(moveBeam(pos, dir));
                }
                break;
        }

        beams.push(...nextBeams);
    }

    return energized.size;
}

function moveBeam(pos: Point, dir: Direction): Beam {
    let newPos = { ...pos };
    switch (dir) {
        case 'up':
            newPos.row--;
            break;
        case 'down':
            newPos.row++;
            break;
        case 'left':
            newPos.col--;
            break;
        case 'right':
            newPos.col++;
            break;
    }
    return { pos: newPos, dir };
}

function reflect(dir: Direction, mirror: '/' | '\\'): Direction {
    if (mirror === '/') {
        switch (dir) {
            case 'up': return 'right';
            case 'down': return 'left';
            case 'left': return 'down';
            case 'right': return 'up';
        }
    } else {
        switch (dir) {
            case 'up': return 'left';
            case 'down': return 'right';
            case 'left': return 'up';
            case 'right': return 'down';
        }
    }
    throw new Error("Invalid direction or mirror");
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const grid = input.split('\n').map(line => line.split(''));
    const result = solve(grid);
    console.log(result);
}

main();
