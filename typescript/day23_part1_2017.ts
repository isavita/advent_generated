import * as fs from 'fs';

type Registers = { [key: string]: number };

const executeInstructions = (instructions: string[]): number => {
    const registers: Registers = { a: 0, b: 0, c: 0, d: 0, e: 0, f: 0, g: 0, h: 0 };
    let mulCount = 0;
    let pointer = 0;

    while (pointer < instructions.length) {
        const [cmd, x, y] = instructions[pointer].split(' ');

        const getValue = (arg: string): number => {
            return isNaN(Number(arg)) ? registers[arg] : Number(arg);
        };

        switch (cmd) {
            case 'set':
                registers[x] = getValue(y);
                break;
            case 'sub':
                registers[x] -= getValue(y);
                break;
            case 'mul':
                registers[x] *= getValue(y);
                mulCount++;
                break;
            case 'jnz':
                if (getValue(x) !== 0) {
                    pointer += getValue(y);
                    continue;
                }
                break;
        }
        pointer++;
    }

    return mulCount;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const result = executeInstructions(input);
    console.log(result);
};

main();