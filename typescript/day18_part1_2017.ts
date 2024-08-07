import * as fs from 'fs';

type Instruction = [string, string, string?];

const parseInstruction = (line: string): Instruction => {
    const parts = line.split(' ');
    return [parts[0], parts[1], parts[2]];
};

const getValue = (arg: string, registers: Map<string, number>): number => {
    return isNaN(Number(arg)) ? registers.get(arg) || 0 : Number(arg);
};

const runDuet = (instructions: Instruction[]) => {
    const registers = new Map<string, number>();
    let lastSound = 0;
    let pointer = 0;

    while (pointer >= 0 && pointer < instructions.length) {
        const [cmd, x, y] = instructions[pointer];

        switch (cmd) {
            case 'snd':
                lastSound = getValue(x, registers);
                break;
            case 'set':
                registers.set(x, getValue(y || '0', registers));
                break;
            case 'add':
                registers.set(x, (registers.get(x) || 0) + getValue(y || '0', registers));
                break;
            case 'mul':
                registers.set(x, (registers.get(x) || 0) * getValue(y || '0', registers));
                break;
            case 'mod':
                registers.set(x, (registers.get(x) || 0) % getValue(y || '0', registers));
                break;
            case 'rcv':
                if (getValue(x, registers) !== 0) {
                    console.log(lastSound);
                    return;
                }
                break;
            case 'jgz':
                if (getValue(x, registers) > 0) {
                    pointer += getValue(y || '0', registers) - 1; // -1 to counter the pointer increment
                }
                break;
        }
        pointer++;
    }
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const instructions = input.map(parseInstruction);
    runDuet(instructions);
};

main();