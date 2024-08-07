import * as fs from 'fs';

enum Direction {
    Up = 0,
    Right = 1,
    Down = 2,
    Left = 3
}

const turnLeft = (dir: Direction): Direction => (dir + 3) % 4;
const turnRight = (dir: Direction): Direction => (dir + 1) % 4;

const move = (x: number, y: number, dir: Direction): [number, number] => {
    switch (dir) {
        case Direction.Up: return [x - 1, y];
        case Direction.Right: return [x, y + 1];
        case Direction.Down: return [x + 1, y];
        case Direction.Left: return [x, y - 1];
    }
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const grid: Map<string, boolean> = new Map();
    const size = input.length;
    const mid = Math.floor(size / 2);
    
    for (let i = 0; i < size; i++) {
        for (let j = 0; j < input[i].length; j++) {
            grid.set(`${i - mid},${j - mid}`, input[i][j] === '#');
        }
    }

    let x = 0, y = 0, direction = Direction.Up, infections = 0;

    for (let burst = 0; burst < 10000; burst++) {
        const posKey = `${x},${y}`;
        if (grid.get(posKey)) {
            direction = turnRight(direction);
            grid.set(posKey, false);
        } else {
            direction = turnLeft(direction);
            grid.set(posKey, true);
            infections++;
        }
        [x, y] = move(x, y, direction);
    }

    console.log(infections);
};

main();