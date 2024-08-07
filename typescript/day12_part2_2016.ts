import * as fs from 'fs';

type Instruction = {
    op: string;
    x: string;
    y?: string;
};

function readInput(filename: string): string[] {
    return fs.readFileSync(filename, 'utf8').split('\n');
}

function parseInstructions(lines: string[]): Instruction[] {
    return lines.map(line => {
        const parts = line.split(' ');
        if (parts.length === 2) {
            return { op: parts[0], x: parts[1] };
        } else {
            return { op: parts[0], x: parts[1], y: parts[2] };
        }
    });
}

function executeInstructions(instructions: Instruction[], initialC: number = 0): number {
    const registers: { [key: string]: number } = { a: 0, b: 0, c: initialC, d: 0 };
    let ip = 0;

    while (ip < instructions.length) {
        const { op, x, y } = instructions[ip];
        const xVal = isNaN(Number(x)) ? registers[x] : Number(x);
        const yVal = y ? (isNaN(Number(y)) ? registers[y] : Number(y)) : 0;

        switch (op) {
            case 'cpy':
                if (y !== undefined) {
                    registers[y] = xVal;
                }
                break;
            case 'inc':
                registers[x]++;
                break;
            case 'dec':
                registers[x]--;
                break;
            case 'jnz':
                if (xVal !== 0) {
                    ip += yVal - 1; // -1 to counteract the ip++ at the end of the loop
                }
                break;
        }
        ip++;
    }

    return registers['a'];
}

function main() {
    const input = readInput('input.txt');
    const instructions = parseInstructions(input);

    // Part One
    const resultPartOne = executeInstructions(instructions);
    console.log(`Part One: ${resultPartOne}`);

    // Part Two
    const resultPartTwo = executeInstructions(instructions, 1);
    console.log(`Part Two: ${resultPartTwo}`);
}

main();