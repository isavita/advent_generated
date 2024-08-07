import * as fs from 'fs';
import * as readline from 'readline';

interface Instruction {
    type: string;
    register: 'a' | 'b' | null;
    offset: number | null;
}

async function readInstructions(filePath: string): Promise<Instruction[]> {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const instructions: Instruction[] = [];

    for await (const line of rl) {
        const parts = line.split(', ');
        const [type, arg] = parts[0].split(' ');

        if (type === 'jmp') {
            instructions.push({ type, register: null, offset: parseInt(arg, 10) });
        } else if (type === 'jie' || type === 'jio') {
            instructions.push({ type, register: arg as 'a' | 'b', offset: parseInt(parts[1], 10) });
        } else {
            instructions.push({ type, register: arg as 'a' | 'b', offset: null });
        }
    }

    return instructions;
}

function executeProgram(instructions: Instruction[]): number {
    const registers = { a: 0, b: 0 };
    let pointer = 0;

    while (pointer >= 0 && pointer < instructions.length) {
        const { type, register, offset } = instructions[pointer];

        switch (type) {
            case 'hlf':
                if (register) registers[register] = Math.floor(registers[register] / 2);
                pointer++;
                break;
            case 'tpl':
                if (register) registers[register] *= 3;
                pointer++;
                break;
            case 'inc':
                if (register) registers[register]++;
                pointer++;
                break;
            case 'jmp':
                if (offset !== null) pointer += offset;
                break;
            case 'jie':
                if (register && registers[register] % 2 === 0 && offset !== null) pointer += offset;
                else pointer++;
                break;
            case 'jio':
                if (register && registers[register] === 1 && offset !== null) pointer += offset;
                else pointer++;
                break;
            default:
                throw new Error(`Unknown instruction type: ${type}`);
        }
    }

    return registers.b;
}

async function main() {
    try {
        const instructions = await readInstructions('input.txt');
        const result = executeProgram(instructions);
        console.log(`The value in register b is: ${result}`);
    } catch (error) {
        console.error('Error reading the file or executing the program:', error);
    }
}

main();