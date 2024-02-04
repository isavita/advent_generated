const fs = require('fs');

class Coord {
    constructor(x, y) {
        this.X = x;
        this.Y = y;
    }
}

class Grid {
    constructor(width, height, data) {
        this.Width = width;
        this.Height = height;
        this.Data = data;
    }

    toString(empty) {
        let result = '';
        for (let y = 0; y < this.Height; y++) {
            for (let x = 0; x < this.Width; x++) {
                const coord = new Coord(x, y);
                if (this.Data[JSON.stringify(coord)]) {
                    result += this.Data[JSON.stringify(coord)];
                } else {
                    result += empty;
                }
            }
            result += '\n';
        }
        return result;
    }

    getEmptyRows() {
        const emptyRows = [];
        for (let y = 0; y < this.Height; y++) {
            let isEmpty = true;
            for (let x = 0; x < this.Width; x++) {
                if (this.Data[JSON.stringify(new Coord(x, y))]) {
                    isEmpty = false;
                    break;
                }
            }
            if (isEmpty) {
                emptyRows.push(y);
            }
        }
        return emptyRows;
    }

    getEmptyCols() {
        const emptyCols = [];
        for (let x = 0; x < this.Width; x++) {
            let isEmpty = true;
            for (let y = 0; y < this.Height; y++) {
                if (this.Data[JSON.stringify(new Coord(x, y))]) {
                    isEmpty = false;
                    break;
                }
            }
            if (isEmpty) {
                emptyCols.push(x);
            }
        }
        return emptyCols;
    }
}

function buildGrid(input, empty) {
    const grid = {
        Width: input[0].length,
        Height: input.length,
        Data: {}
    };

    for (let y = 0; y < input.length; y++) {
        for (let x = 0; x < input[y].length; x++) {
            if (input[y][x] !== empty) {
                grid.Data[JSON.stringify(new Coord(x, y))] = input[y][x];
            }
        }
    }

    return new Grid(grid.Width, grid.Height, grid.Data);
}

function calculateOffsets(emptyIndexes, bound) {
    const offsets = new Array(bound).fill(0);
    for (const idx of emptyIndexes) {
        for (let i = idx + 1; i < offsets.length; i++) {
            offsets[i]++;
        }
    }
    return offsets;
}

function expandGrid(grid, expansionFactor) {
    const emptyCols = grid.getEmptyCols();
    const emptyRows = grid.getEmptyRows();
    const numLinesToAdd = expansionFactor - 1;

    const newGrid = new Grid(grid.Width + emptyCols.length * numLinesToAdd, grid.Height + emptyRows.length * numLinesToAdd, {});

    const dXs = calculateOffsets(emptyCols, grid.Width);
    const dYs = calculateOffsets(emptyRows, grid.Height);

    for (let y = 0; y < grid.Height; y++) {
        for (let x = 0; x < grid.Width; x++) {
            const coord = new Coord(x, y);
            if (grid.Data[JSON.stringify(coord)]) {
                const newCoord = new Coord(x + dXs[x] * numLinesToAdd, y + dYs[y] * numLinesToAdd);
                newGrid.Data[JSON.stringify(newCoord)] = grid.Data[JSON.stringify(coord)];
            }
        }
    }

    return newGrid;
}

function abs(x) {
    return x < 0 ? -x : x;
}

function calculateLength(grid, c1, c2) {
    const dX = abs(c2.X - c1.X);
    const dY = abs(c2.Y - c1.Y);
    return dX + dY;
}

function solve(input, expansionFactor) {
    const grid = buildGrid(input, '.');

    const expandedGrid = expandGrid(grid, expansionFactor);

    let res = 0;
    const alreadySeen = {};
    for (const coord1 in expandedGrid.Data) {
        for (const coord2 in alreadySeen) {
            const length = calculateLength(expandedGrid, JSON.parse(coord1), JSON.parse(coord2));
            res += length;
        }
        alreadySeen[coord1] = true;
    }

    return res;
}

function readFile(fileName) {
    const file = fs.readFileSync(fileName, 'utf8');
    return file.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input, 1000000));