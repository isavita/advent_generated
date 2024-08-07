import * as fs from 'fs';

type Instruction = { operation: string; argument: number };

const readInstructions = (filePath: string): Instruction[] => {
    return fs.readFileSync(filePath, 'utf-8')
        .trim()
        .split('\n')
        .map(line => {
            const [operation, argument] = line.split(' ');
            return { operation, argument: parseInt(argument) };
        });
};

const executeInstructions = (instructions: Instruction[]): { accumulator: number, terminated: boolean } => {
    let accumulator = 0;
    const visited = new Set<number>();
    let pointer = 0;

    while (pointer < instructions.length) {
        if (visited.has(pointer)) return { accumulator, terminated: false };
        visited.add(pointer);

        const { operation, argument } = instructions[pointer];
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
    return { accumulator, terminated: true };
};

const findTerminatingModification = (instructions: Instruction[]): number => {
    for (let i = 0; i < instructions.length; i++) {
        const original = instructions[i];
        if (original.operation === 'acc') continue;

        // Modify the instruction
        instructions[i] = { operation: original.operation === 'nop' ? 'jmp' : 'nop', argument: original.argument };

        const { accumulator, terminated } = executeInstructions(instructions);
        if (terminated) {
            return accumulator;
        }

        // Revert the modification
        instructions[i] = original;
    }
    return 0; // Should not reach here
};

const main = () => {
    const instructions = readInstructions('input.txt');
    const { accumulator } = executeInstructions(instructions);
    console.log(`Accumulator before repeat: ${accumulator}`);

    const fixedAccumulator = findTerminatingModification(instructions);
    console.log(`Accumulator after fixing: ${fixedAccumulator}`);
};

main();