import * as fs from 'fs';

function readInput(filePath: string): number[] {
    const data = fs.readFileSync(filePath, 'utf8');
    return data.split('\n').map(Number).filter(num => !isNaN(num));
}

function solvePart1(instructions: number[]): number {
    let steps = 0;
    let currentIndex = 0;

    while (currentIndex >= 0 && currentIndex < instructions.length) {
        let offset = instructions[currentIndex];
        instructions[currentIndex]++;
        currentIndex += offset;
        steps++;
    }

    return steps;
}

function solvePart2(instructions: number[]): number {
    let steps = 0;
    let currentIndex = 0;

    while (currentIndex >= 0 && currentIndex < instructions.length) {
        let offset = instructions[currentIndex];
        instructions[currentIndex] += offset >= 3 ? -1 : 1;
        currentIndex += offset;
        steps++;
    }

    return steps;
}

const inputFilePath = 'input.txt';
const instructions = readInput(inputFilePath);

const part1Steps = solvePart1([...instructions]);
console.log(`Part 1: Steps to reach the exit: ${part1Steps}`);

const part2Steps = solvePart2([...instructions]);
console.log(`Part 2: Steps to reach the exit: ${part2Steps}`);