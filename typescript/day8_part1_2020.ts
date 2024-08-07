import * as fs from 'fs';

type Instruction = [string, number];

const parseInstructions = (data: string): Instruction[] => {
    return data.trim().split('\n').map(line => {
        const [operation, argument] = line.split(' ');
        return [operation, parseInt(argument)];
    });
};

const runProgram = (instructions: Instruction[]): number => {
    const visited = new Set<number>();
    let accumulator = 0;
    let pointer = 0;

    while (pointer < instructions.length) {
        if (visited.has(pointer)) {
            return accumulator;
        }
        visited.add(pointer);
        
        const [operation, argument] = instructions[pointer];
        switch (operation) {
            case 'acc':
                accumulator += argument;
                pointer++;
                break;
            case 'jmp':
                pointer += argument;
                break;
            case 'nop':
                pointer++;
                break;
        }
    }
    return accumulator;
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const instructions = parseInstructions(data);
    const result = runProgram(instructions);
    console.log(result);
};

main();