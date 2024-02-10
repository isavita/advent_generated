
const fs = require('fs');

const screenWidth = 50;
const screenHeight = 6;

function main() {
    const input = fs.readFileSync('input.txt', 'utf8').split('\n');
    const screen = Array.from({ length: screenHeight }, () => Array(screenWidth).fill(false));

    input.forEach(instruction => processInstruction(instruction, screen));

    displayScreen(screen);
}

function displayScreen(screen) {
    screen.forEach(row => {
        row.forEach(pixel => {
            process.stdout.write(pixel ? '#' : '.');
        });
        process.stdout.write('\n');
    });
}

function processInstruction(instruction, screen) {
    const rectRegex = /rect (\d+)x(\d+)/;
    const rotateRowRegex = /rotate row y=(\d+) by (\d+)/;
    const rotateColumnRegex = /rotate column x=(\d+) by (\d+)/;

    if (rectRegex.test(instruction)) {
        const [, a, b] = instruction.match(rectRegex).map(Number);
        rect(screen, a, b);
    } else if (rotateRowRegex.test(instruction)) {
        const [, a, b] = instruction.match(rotateRowRegex).map(Number);
        rotateRow(screen, a, b);
    } else if (rotateColumnRegex.test(instruction)) {
        const [, a, b] = instruction.match(rotateColumnRegex).map(Number);
        rotateColumn(screen, a, b);
    }
}

function rect(screen, a, b) {
    for (let y = 0; y < b; y++) {
        for (let x = 0; x < a; x++) {
            screen[y][x] = true;
        }
    }
}

function rotateRow(screen, row, shift) {
    const temp = Array(screenWidth).fill(false);
    screen[row].forEach((_, i) => {
        temp[(i + shift) % screenWidth] = screen[row][i];
    });
    screen[row] = temp;
}

function rotateColumn(screen, col, shift) {
    const temp = Array(screenHeight).fill(false);
    screen.forEach((_, i) => {
        temp[(i + shift) % screenHeight] = screen[i][col];
    });
    screen.forEach((_, i) => {
        screen[i][col] = temp[i];
    });
}

function countLitPixels(screen) {
    return screen.flat().filter(pixel => pixel).length;
}

main();
