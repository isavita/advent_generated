import * as fs from 'fs';

// Function to read the input file
function readInput(filePath: string): string[] {
    return fs.readFileSync(filePath, 'utf-8').split('\n').filter(line => line.trim() !== '');
}

// Function to create a new screen
function createScreen(width: number, height: number): boolean[][] {
    const screen: boolean[][] = [];
    for (let i = 0; i < height; i++) {
        screen.push(new Array(width).fill(false));
    }
    return screen;
}

// Function to execute a single command
function executeCommand(screen: boolean[][], command: string) {
    const parts = command.split(' ');
    if (parts[0] === 'rect') {
        const [width, height] = parts[1].split('x').map(Number);
        for (let i = 0; i < height; i++) {
            for (let j = 0; j < width; j++) {
                screen[i][j] = true;
            }
        }
    } else if (parts[1] === 'row') {
        const row = parseInt(parts[2].split('=')[1]);
        const by = parseInt(parts[4]);
        const newRow = [...screen[row]];
        for (let i = 0; i < screen[row].length; i++) {
            screen[row][(i + by) % screen[row].length] = newRow[i];
        }
    } else if (parts[1] === 'column') {
        const col = parseInt(parts[2].split('=')[1]);
        const by = parseInt(parts[4]);
        const newCol: boolean[] = [];
        for (let i = 0; i < screen.length; i++) {
            newCol.push(screen[i][col]);
        }
        for (let i = 0; i < screen.length; i++) {
            screen[(i + by) % screen.length][col] = newCol[i];
        }
    }
}

// Function to count lit pixels
function countLitPixels(screen: boolean[][]): number {
    return screen.flat().filter(pixel => pixel).length;
}

// Function to print the screen
function printScreen(screen: boolean[][]) {
    for (let row of screen) {
        console.log(row.map(pixel => pixel ? '#' : '.').join(''));
    }
}

// Main function
function main() {
    const input = readInput('input.txt');
    const screen = createScreen(50, 6);

    for (let command of input) {
        executeCommand(screen, command);
    }

    const litPixels = countLitPixels(screen);
    console.log(`Number of lit pixels: ${litPixels}`);
    console.log('Screen display:');
    printScreen(screen);
}

main();