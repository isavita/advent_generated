const fs = require('fs');

class Coord {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
}

class Grid {
    constructor(width, height, data) {
        this.width = width;
        this.height = height;
        this.data = data;
    }

    toString(empty) {
        let result = '';
        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                const coord = new Coord(x, y);
                if (this.data[JSON.stringify(coord)]) {
                    result += this.data[JSON.stringify(coord)];
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
        for (let y = 0; y < this.height; y++) {
            let isEmpty = true;
            let x = 0;
            while (x < this.width) {
                if (this.data[JSON.stringify(new Coord(x, y))]) {
                    isEmpty = false;
                }
                x++;
            }
            if (isEmpty) {
                emptyRows.push(y);
            }
        }
        return emptyRows;
    }

    getEmptyCols() {
        const emptyCols = [];
        for (let x = 0; x < this.width; x++) {
            let isEmpty = true;
            let y = 0;
            while (y < this.height) {
                if (this.data[JSON.stringify(new Coord(x, y))]) {
                    isEmpty = false;
                }
                y++;
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
        width: input[0].length,
        height: input.length,
        data: {}
    };

    for (let y = 0; y < input.length; y++) {
        for (let x = 0; x < input[y].length; x++) {
            if (input[y][x] !== empty) {
                grid.data[JSON.stringify(new Coord(x, y))] = input[y][x];
            }
        }
    }

    return new Grid(grid.width, grid.height, grid.data);
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

    const newGrid = new Grid(grid.width + emptyCols.length * numLinesToAdd, grid.height + emptyRows.length * numLinesToAdd, {});

    const dXs = calculateOffsets(emptyCols, grid.width);
    const dYs = calculateOffsets(emptyRows, grid.height);

    for (let y = 0; y < grid.height; y++) {
        for (let x = 0; x < grid.width; x++) {
            const coord = new Coord(x, y);
            if (grid.data[JSON.stringify(coord)]) {
                const newCoord = new Coord(x + dXs[x] * numLinesToAdd, y + dYs[y] * numLinesToAdd);
                newGrid.data[JSON.stringify(newCoord)] = grid.data[JSON.stringify(coord)];
            }
        }
    }

    return newGrid;
}

function abs(x) {
    return x < 0 ? -x : x;
}

function calculateLength(grid, c1, c2) {
    const dX = abs(c2.x - c1.x);
    const dY = abs(c2.y - c1.y);
    return dX + dY;
}

function solve(input) {
    const grid = buildGrid(input, '.');

    const expandedGrid = expandGrid(grid, 2);

    let res = 0;
    const alreadySeen = {};
    for (const coord1 of Object.keys(expandedGrid.data)) {
        for (const coord2 of Object.keys(alreadySeen)) {
            const length = calculateLength(expandedGrid, JSON.parse(coord1), JSON.parse(coord2));
            res += length;
        }
        alreadySeen[coord1] = true;
    }

    return res;
}

function readFile(fileName) {
    const input = fs.readFileSync(fileName, 'utf8').trim().split('\n');
    return input;
}

const input = readFile('input.txt');
console.log(solve(input));