import * as fs from 'fs';
import * as path from 'path';

function readInputFile(filePath: string): number[] {
    const fileContent = fs.readFileSync(filePath, 'utf-8');
    return fileContent.split('\n').filter(line => line.trim() !== '').map(Number);
}

function solveMaze(jumps: number[]): number {
    let steps = 0;
    let index = 0;

    while (index >= 0 && index < jumps.length) {
        const offset = jumps[index];
        jumps[index]++;
        index += offset;
        steps++;
    }

    return steps;
}

function main() {
    const filePath = path.join(__dirname, 'input.txt');
    const jumps = readInputFile(filePath);
    const steps = solveMaze(jumps);
    console.log(`It takes ${steps} steps to reach the exit.`);
}

main();