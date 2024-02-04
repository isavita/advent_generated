const fs = require('fs');

class Coord {
    constructor(x, y) {
        this.X = x;
        this.Y = y;
    }

    Add(c2) {
        return new Coord(this.X + c2.X, this.Y + c2.Y);
    }

    isInBounds(grid) {
        return 0 <= this.X && this.X < grid.Width && 0 <= this.Y && this.Y < grid.Height;
    }
}

class Grid {
    constructor(width, height, data) {
        this.Width = width;
        this.Height = height;
        this.Data = data;
    }

    toString() {
        let result = '';

        for (let y = 0; y < this.Height; y++) {
            for (let x = 0; x < this.Width; x++) {
                const coord = new Coord(x, y);
                if (this.Data[JSON.stringify(coord)]) {
                    result += this.Data[JSON.stringify(coord)];
                } else {
                    result += '.';
                }
            }
            result += '\n';
        }

        return result;
    }
}

const Empty = '.';
const CubicRock = '#';
const RoundRock = 'O';

const North = new Coord(0, -1);
const West = new Coord(-1, 0);
const South = new Coord(0, 1);
const East = new Coord(1, 0);

function buildGrid(input) {
    const grid = {
        Width: input[0].length,
        Height: input.length,
        Data: {}
    };

    for (let y = 0; y < input.length; y++) {
        for (let x = 0; x < input[y].length; x++) {
            if (input[y][x] !== Empty) {
                grid.Data[JSON.stringify(new Coord(x, y))] = input[y][x];
            }
        }
    }

    return new Grid(grid.Width, grid.Height, grid.Data);
}

function shiftSingleRock(grid, coord, dir) {
    if (grid.Data[JSON.stringify(coord)] === RoundRock) {
        let current = coord;
        let before = coord.Add(dir);

        while (!grid.Data[JSON.stringify(before)] && before.isInBounds(grid)) {
            grid.Data[JSON.stringify(before)] = RoundRock;
            delete grid.Data[JSON.stringify(current)];

            current = before;
            before = before.Add(dir);
        }
    }
}

function shiftRocks(grid, dir) {
    switch (dir) {
        case North:
        case West:
            for (let x = 0; x < grid.Width; x++) {
                for (let y = 0; y < grid.Height; y++) {
                    shiftSingleRock(grid, new Coord(x, y), dir);
                }
            }
            break;

        case South:
        case East:
            for (let x = grid.Width - 1; x >= 0; x--) {
                for (let y = grid.Height - 1; y >= 0; y--) {
                    shiftSingleRock(grid, new Coord(x, y), dir);
                }
            }
            break;
    }
}

function calculateLoad(grid) {
    let load = 0;

    for (let x = 0; x < grid.Width; x++) {
        for (let y = 0; y < grid.Height; y++) {
            const coord = new Coord(x, y);
            if (grid.Data[JSON.stringify(coord)] === RoundRock) {
                load += grid.Height - y;
            }
        }
    }

    return load;
}

function solve(input) {
    const grid = buildGrid(input);
    shiftRocks(grid, North);

    return calculateLoad(grid);
}

function readFile(fileName) {
    const file = fs.readFileSync(fileName, 'utf8');
    return file.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));