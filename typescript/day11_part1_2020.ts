import * as fs from 'fs';

const directions = [
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1],          [0, 1],
    [1, -1], [1, 0], [1, 1],
];

function readInput(filePath: string): string[][] {
    const data = fs.readFileSync(filePath, 'utf-8').trim();
    return data.split('\n').map(line => line.split(''));
}

function countOccupiedAdjacent(seats: string[][], row: number, col: number): number {
    return directions.reduce((count, [dx, dy]) => {
        const x = row + dx;
        const y = col + dy;
        if (seats[x]?.[y] === '#') count++;
        return count;
    }, 0);
}

function simulateSeating(seats: string[][]): string[][] {
    const newSeats = seats.map(row => [...row]);
    let changed = false;

    for (let i = 0; i < seats.length; i++) {
        for (let j = 0; j < seats[i].length; j++) {
            if (seats[i][j] === 'L' && countOccupiedAdjacent(seats, i, j) === 0) {
                newSeats[i][j] = '#';
                changed = true;
            } else if (seats[i][j] === '#' && countOccupiedAdjacent(seats, i, j) >= 4) {
                newSeats[i][j] = 'L';
                changed = true;
            }
        }
    }
    return changed ? newSeats : seats;
}

function countOccupiedSeats(seats: string[][]): number {
    return seats.flat().filter(seat => seat === '#').length;
}

function main() {
    const seats = readInput('input.txt');
    let currentSeats = seats;

    while (true) {
        const nextSeats = simulateSeating(currentSeats);
        if (nextSeats === currentSeats) break;
        currentSeats = nextSeats;
    }

    console.log(countOccupiedSeats(currentSeats));
}

main();