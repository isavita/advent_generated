import * as fs from 'fs';

const rockstr = `####

 #
###
 #

  #
  #
###

#
#
#
#

##
##`;

enum Dir {
    N,
    E,
    S,
    W
}

const point = {
    [Dir.N]: { x: 0, y: 1 },
    [Dir.E]: { x: 1, y: 0 },
    [Dir.S]: { x: 0, y: -1 },
    [Dir.W]: { x: -1, y: 0 }
};

const fromByte = {
    'N': Dir.N,
    'E': Dir.E,
    'S': Dir.S,
    'W': Dir.W,
    'U': Dir.N,
    'R': Dir.E,
    'D': Dir.S,
    'L': Dir.W,
    '^': Dir.N,
    '>': Dir.E,
    'v': Dir.S,
    '<': Dir.W,
} as const;

function dirFromByte(b: string): Dir {
    return fromByte[b as keyof typeof fromByte];
}

function getRocks(): Array<{ [key: string]: boolean }> {
    const rocks: Array<{ [key: string]: boolean }> = [];
    rockstr.split('\n\n').forEach((rock, i) => {
        rocks[i] = {};
        const lines = rock.split('\n');
        lines.forEach((line, y) => {
            for (let x = 0; x < line.length; x++) {
                if (line[x] === '#') {
                    rocks[i][`${x},${lines.length - 1 - y}`] = true;
                }
            }
        });
    });
    return rocks;
}

function collision(grid: { [key: string]: boolean }, rock: { [key: string]: boolean }, pos: { x: number, y: number }): boolean {
    for (const p in rock) {
        const [px, py] = p.split(',').map(Number);
        const newX = px + pos.x;
        const newY = py + pos.y;
        if (grid[`${newX},${newY}`] || newX < 0 || newX > 6) {
            return true;
        }
    }
    return false;
}

function main() {
    const jetPattern = fs.readFileSync('input.txt', 'utf8').trim().split('');
    const rocks = getRocks();
    const grid: { [key: string]: boolean } = {};
    for (let x = 0; x < 7; x++) {
        grid[`${x},0`] = true;
    }
    let floor = 0, j = 0;
    const repeat: { [key: string]: [number, number] } = {};

    for (let i = 0, curr = 0; ; i++, curr = (curr + 1) % rocks.length) {
        const key = `${curr},${j}`;
        if (repeat[key]) {
            const [previ, prevFloor] = repeat[key];
            if ((1000000000000 - i) % (i - previ) === 0) {
                console.log(floor + (1000000000000 - i) / (i - previ) * (floor - prevFloor));
                break;
            }
        }
        repeat[key] = [i, floor];
        const currRock = rocks[curr];
        let pos = { x: 2, y: floor + 4 };
        while (true) {
            const jet = jetPattern[j];
            j = (j + 1) % jetPattern.length;
            pos = { x: pos.x + point[dirFromByte(jet)].x, y: pos.y + point[dirFromByte(jet)].y };
            if (collision(grid, currRock, pos)) {
                pos = { x: pos.x - point[dirFromByte(jet)].x, y: pos.y - point[dirFromByte(jet)].y };
            }
            pos = { x: pos.x + point[Dir.S].x, y: pos.y + point[Dir.S].y };
            if (collision(grid, currRock, pos)) {
                pos = { x: pos.x - point[Dir.S].x, y: pos.y - point[Dir.S].y };
                for (const p in currRock) {
                    const [px, py] = p.split(',').map(Number);
                    const newX = px + pos.x;
                    const newY = py + pos.y;
                    grid[`${newX},${newY}`] = true;
                    if (newY > floor) {
                        floor = newY;
                    }
                }
                break;
            }
        }
    }
}

main();