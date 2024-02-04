const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(line => line.split(''));

let seatingArea = input;
let stabilized = false;

while (!stabilized) {
    const result = simulateSeating(seatingArea);
    seatingArea = result[0];
    stabilized = result[1];
}

console.log(countOccupiedSeats(seatingArea));

function simulateSeating(seatingArea) {
    const rows = seatingArea.length;
    const cols = seatingArea[0].length;
    const newSeatingArea = new Array(rows).fill(null).map(() => new Array(cols));
    let stabilized = true;

    for (let i = 0; i < rows; i++) {
        for (let j = 0; j < cols; j++) {
            switch (seatingArea[i][j]) {
                case 'L':
                    if (countAdjacentOccupied(seatingArea, i, j) === 0) {
                        newSeatingArea[i][j] = '#';
                        stabilized = false;
                    } else {
                        newSeatingArea[i][j] = 'L';
                    }
                    break;
                case '#':
                    if (countAdjacentOccupied(seatingArea, i, j) >= 4) {
                        newSeatingArea[i][j] = 'L';
                        stabilized = false;
                    } else {
                        newSeatingArea[i][j] = '#';
                    }
                    break;
                default:
                    newSeatingArea[i][j] = '.';
            }
        }
    }

    return [newSeatingArea, stabilized];
}

function countAdjacentOccupied(seatingArea, row, col) {
    let count = 0;
    for (let i = row - 1; i <= row + 1; i++) {
        for (let j = col - 1; j <= col + 1; j++) {
            if (i === row && j === col) continue;
            if (i >= 0 && i < seatingArea.length && j >= 0 && j < seatingArea[0].length) {
                if (seatingArea[i][j] === '#') {
                    count++;
                }
            }
        }
    }
    return count;
}

function countOccupiedSeats(seatingArea) {
    let count = 0;
    seatingArea.forEach(row => {
        row.forEach(seat => {
            if (seat === '#') {
                count++;
            }
        });
    });
    return count;
}