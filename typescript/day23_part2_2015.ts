import * as fs from 'fs';
import * as readline from 'readline';

interface Instruction {
    type: string;
    arg1: string;
    arg2?: string;
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
        if (parts.length === 1) {
            const [type, arg1] = line.split(' ');
            instructions.push({ type, arg1 });
        } else {
            const [typeArg1, arg2] = parts;
            const [type, arg1] = typeArg1.split(' ');
            instructions.push({ type, arg1, arg2 });
        }
    }

    return instructions;
}

function executeProgram(instructions: Instruction[], initialA: number = 0): number {
    let a = initialA;
    let b = 0;
    let i = 0;

    while (i >= 0 && i < instructions.length) {
        const instruction = instructions[i];
        switch (instruction.type) {
            case 'hlf':
                if (instruction.arg1 === 'a') a = Math.floor(a / 2);
                else b = Math.floor(b / 2);
                i++;
                break;
            case 'tpl':
                if (instruction.arg1 === 'a') a *= 3;
                else b *= 3;
                i++;
                break;
            case 'inc':
                if (instruction.arg1 === 'a') a++;
                else b++;
                i++;
                break;
            case 'jmp':
                i += parseInt(instruction.arg1);
                break;
            case 'jie':
                if ((instruction.arg1 === 'a' && a % 2 === 0) || (instruction.arg1 === 'b' && b % 2 === 0)) {
                    i += parseInt(instruction.arg2!);
                } else {
                    i++;
                }
                break;
            case 'jio':
                if ((instruction.arg1 === 'a' && a === 1) || (instruction.arg1 === 'b' && b === 1)) {
                    i += parseInt(instruction.arg2!);
                } else {
                    i++;
                }
                break;
            default:
                throw new Error(`Unknown instruction type: ${instruction.type}`);
        }
    }

    return b;
}

(async () => {
    const instructions = await readInstructions('input.txt');
    const resultPart1 = executeProgram(instructions);
    console.log(`Value in register b (Part 1): ${resultPart1}`);

    const resultPart2 = executeProgram(instructions, 1);
    console.log(`Value in register b (Part 2): ${resultPart2}`);
})();