import * as fs from 'fs';

const solve = (input: string): number => {
    const opcodeComputer = parseInput(input);

    let lastReg5: number = 0; // Initialize with a default value
    const comparedRegister5s = new Set<number>();
    while (!opcodeComputer.tick()) {
        if (opcodeComputer.registers[opcodeComputer.instructionPointer] === 28) {
            const reg5 = opcodeComputer.registers[5];
            if (comparedRegister5s.has(reg5)) {
                break;
            }
            comparedRegister5s.add(reg5);
            lastReg5 = reg5;
        }
    }

    return lastReg5;
};

class OpcodeComputer {
    instructions: Instruction[];
    registers: number[];
    instructionPointer: number;

    constructor(instructions: Instruction[], instructionPointer: number) {
        this.instructions = instructions;
        this.registers = new Array(6).fill(0);
        this.instructionPointer = instructionPointer;
    }

    tick(): boolean {
        if (this.registers[this.instructionPointer] >= this.instructions.length) {
            console.log("Out of range instruction, terminating...");
            return true;
        }
        const instIndex = this.registers[this.instructionPointer];
        const inst = this.instructions[instIndex];

        const opcodeFunc = opcodeNamesToFuncs[inst.name];

        this.registers = opcodeFunc(this.registers, inst.abcValues);

        this.registers[this.instructionPointer]++;

        if (this.registers[this.instructionPointer] >= this.instructions.length) {
            return true;
        }

        return false;
    }
}

interface Instruction {
    name: string;
    abcValues: number[];
}

const parseInput = (input: string): OpcodeComputer => {
    const lines = input.split('\n');

    const instructionPointer = parseInt(lines[0].split(' ')[1]);

    const instructions: Instruction[] = lines.slice(1).map(line => {
        const [name, a, b, c] = line.split(' ');
        return { name, abcValues: [parseInt(a), parseInt(b), parseInt(c)] };
    });

    return new OpcodeComputer(instructions, instructionPointer);
};

const addr = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] + registers[abcValues[1]];
    return registers;
};

const addi = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] + abcValues[1];
    return registers;
};

const mulr = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] * registers[abcValues[1]];
    return registers;
};

const muli = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] * abcValues[1];
    return registers;
};

const banr = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] & registers[abcValues[1]];
    return registers;
};

const bani = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] & abcValues[1];
    return registers;
};

const borr = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] | registers[abcValues[1]];
    return registers;
};

const bori = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] | abcValues[1];
    return registers;
};

const setr = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]];
    return registers;
};

const seti = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = abcValues[0];
    return registers;
};

const gtir = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = abcValues[0] > registers[abcValues[1]] ? 1 : 0;
    return registers;
};

const gtri = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] > abcValues[1] ? 1 : 0;
    return registers;
};

const gtrr = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] > registers[abcValues[1]] ? 1 : 0;
    return registers;
};

const eqir = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = abcValues[0] === registers[abcValues[1]] ? 1 : 0;
    return registers;
};

const eqri = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] === abcValues[1] ? 1 : 0;
    return registers;
};

const eqrr = (registers: number[], abcValues: number[]): number[] => {
    registers[abcValues[2]] = registers[abcValues[0]] === registers[abcValues[1]] ? 1 : 0;
    return registers;
};

const opcodeNamesToFuncs: { [key: string]: (registers: number[], abcValues: number[]) => number[] } = {
    "addr": addr, "addi": addi,
    "mulr": mulr, "muli": muli,
    "banr": banr, "bani": bani,
    "borr": borr, "bori": bori,
    "setr": setr, "seti": seti,
    "gtir": gtir, "gtri": gtri, "gtrr": gtrr,
    "eqir": eqir, "eqri": eqri, "eqrr": eqrr,
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf8');
    console.log(solve(input));
};

main();