import * as fs from 'fs';

type Intcode = number[];
type PhaseSetting = number[];

const runIntcode = (program: Intcode, inputs: number[]): number => {
    const memory = [...program];
    let pointer = 0;
    let inputIndex = 0;

    while (true) {
        const instruction = memory[pointer] % 100;
        const modes = [
            Math.floor(memory[pointer] / 100) % 10,
            Math.floor(memory[pointer] / 1000) % 10,
            Math.floor(memory[pointer] / 10000) % 10,
        ];

        const getParam = (offset: number): number => {
            const mode = modes[offset - 1];
            return mode === 0 ? memory[memory[pointer + offset]] : memory[pointer + offset];
        };

        switch (instruction) {
            case 1:
                memory[memory[pointer + 3]] = getParam(1) + getParam(2);
                pointer += 4;
                break;
            case 2:
                memory[memory[pointer + 3]] = getParam(1) * getParam(2);
                pointer += 4;
                break;
            case 3:
                if (inputIndex < inputs.length) {
                    memory[memory[pointer + 1]] = inputs[inputIndex++];
                } else {
                    return -1; // Wait for input
                }
                pointer += 2;
                break;
            case 4:
                return getParam(1);
            case 5:
                pointer = getParam(1) !== 0 ? getParam(2) : pointer + 3;
                break;
            case 6:
                pointer = getParam(1) === 0 ? getParam(2) : pointer + 3;
                break;
            case 7:
                memory[memory[pointer + 3]] = getParam(1) < getParam(2) ? 1 : 0;
                pointer += 4;
                break;
            case 8:
                memory[memory[pointer + 3]] = getParam(1) === getParam(2) ? 1 : 0;
                pointer += 4;
                break;
            case 99:
                return -1; // Halt
            default:
                throw new Error(`Unknown instruction ${instruction}`);
        }
    }
};

const getMaxThrusterSignal = (program: Intcode): number => {
    const phaseSettings = [0, 1, 2, 3, 4];
    const permutations = (arr: number[]): number[][] => {
        if (arr.length === 0) return [[]];
        return arr.flatMap((v, i) =>
            permutations(arr.filter((_, j) => j !== i)).map((p) => [v, ...p])
        );
    };

    const settings = permutations(phaseSettings);
    let maxSignal = 0;

    for (const setting of settings) {
        let inputSignal = 0;
        for (const phase of setting) {
            inputSignal = runIntcode(program, [phase, inputSignal]);
        }
        maxSignal = Math.max(maxSignal, inputSignal);
    }

    return maxSignal;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const program: Intcode = input.split(',').map(Number);
    const maxSignal = getMaxThrusterSignal(program);
    console.log(maxSignal);
};

main();