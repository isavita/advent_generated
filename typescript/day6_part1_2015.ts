import * as fs from 'fs';
import * as readline from 'readline';

// Function to parse the command and update the light grid
function processCommand(command: string, grid: boolean[][]): void {
    const regex = /(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)/;
    const match = command.match(regex);

    if (!match) {
        throw new Error(`Invalid command: ${command}`);
    }

    const action = match[1];
    const startX = parseInt(match[2], 10);
    const startY = parseInt(match[3], 10);
    const endX = parseInt(match[4], 10);
    const endY = parseInt(match[5], 10);

    for (let x = startX; x <= endX; x++) {
        for (let y = startY; y <= endY; y++) {
            if (action === 'turn on') {
                grid[x][y] = true;
            } else if (action === 'turn off') {
                grid[x][y] = false;
            } else if (action === 'toggle') {
                grid[x][y] = !grid[x][y];
            }
        }
    }
}

// Function to count the number of lights that are on
function countLightsOn(grid: boolean[][]): number {
    let count = 0;
    for (let x = 0; x < 1000; x++) {
        for (let y = 0; y < 1000; y++) {
            if (grid[x][y]) {
                count++;
            }
        }
    }
    return count;
}

// Main function to read the file and process the commands
async function main() {
    const fileStream = fs.createReadStream('input.txt');
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const grid: boolean[][] = Array.from({ length: 1000 }, () => Array(1000).fill(false));

    for await (const line of rl) {
        processCommand(line, grid);
    }

    const lightsOn = countLightsOn(grid);
    console.log(`Number of lights lit: ${lightsOn}`);
}

main().catch(console.error);