import * as fs from 'fs';

const runIntcode = (program: number[]): number[] => {
    const memory = [...program];
    let pointer = 0;

    while (true) {
        const opcode = memory[pointer];
        if (opcode === 99) break;

        const [param1, param2, outputPos] = memory.slice(pointer + 1, pointer + 4);
        if (opcode === 1) {
            memory[outputPos] = memory[param1] + memory[param2];
        } else if (opcode === 2) {
            memory[outputPos] = memory[param1] * memory[param2];
        }

        pointer += 4;
    }
    return memory;
};

const findNounAndVerb = (program: number[], target: number): [number, number] => {
    for (let noun = 0; noun <= 99; noun++) {
        for (let verb = 0; verb <= 99; verb++) {
            const modifiedProgram = [...program];
            modifiedProgram[1] = noun;
            modifiedProgram[2] = verb;
            const output = runIntcode(modifiedProgram)[0];
            if (output === target) {
                return [noun, verb];
            }
        }
    }
    throw new Error('No valid noun and verb found');
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const program = input.split(',').map(Number);
    
    // Part 1
    const initialProgram = [...program];
    initialProgram[1] = 12;
    initialProgram[2] = 2;
    const resultPart1 = runIntcode(initialProgram)[0];
    console.log('Part 1 Result:', resultPart1);

    // Part 2
    const targetOutput = 19690720;
    const [noun, verb] = findNounAndVerb(program, targetOutput);
    const resultPart2 = 100 * noun + verb;
    console.log('Part 2 Result:', resultPart2);
};

main();