import * as fs from 'fs';

class IntcodeComputer {
    private memory: number[];
    private pointer: number;
    private relativeBase: number;

    constructor(program: number[]) {
        this.memory = [...program];
        this.pointer = 0;
        this.relativeBase = 0;
    }

    private getParam(mode: number, offset: number): number {
        const param = this.memory[this.pointer + offset] || 0;
        switch (mode) {
            case 0: return this.memory[param] || 0; // Position mode
            case 1: return param; // Immediate mode
            case 2: return this.memory[this.relativeBase + param] || 0; // Relative mode
        }
        throw new Error(`Unknown mode: ${mode}`);
    }

    private setParam(mode: number, offset: number, value: number): void {
        const param = this.memory[this.pointer + offset] || 0;
        switch (mode) {
            case 0: this.memory[param] = value; break; // Position mode
            case 2: this.memory[this.relativeBase + param] = value; break; // Relative mode
            default: throw new Error(`Unknown mode for writing: ${mode}`);
        }
    }

    public run(input: number): number {
        while (true) {
            const instruction = this.memory[this.pointer] || 0;
            const opcode = instruction % 100;
            const modes = [
                Math.floor(instruction / 100) % 10,
                Math.floor(instruction / 1000) % 10,
                Math.floor(instruction / 10000) % 10,
            ];

            switch (opcode) {
                case 1: // Add
                    this.setParam(modes[2], 3, this.getParam(modes[0], 1) + this.getParam(modes[1], 2));
                    this.pointer += 4;
                    break;
                case 2: // Multiply
                    this.setParam(modes[2], 3, this.getParam(modes[0], 1) * this.getParam(modes[1], 2));
                    this.pointer += 4;
                    break;
                case 3: // Input
                    this.setParam(modes[0], 1, input);
                    this.pointer += 2;
                    break;
                case 4: // Output
                    return this.getParam(modes[0], 1);
                case 5: // Jump-if-true
                    this.pointer = this.getParam(modes[0], 1) !== 0 ? this.getParam(modes[1], 2) : this.pointer + 3;
                    break;
                case 6: // Jump-if-false
                    this.pointer = this.getParam(modes[0], 1) === 0 ? this.getParam(modes[1], 2) : this.pointer + 3;
                    break;
                case 7: // Less than
                    this.setParam(modes[2], 3, this.getParam(modes[0], 1) < this.getParam(modes[1], 2) ? 1 : 0);
                    this.pointer += 4;
                    break;
                case 8: // Equals
                    this.setParam(modes[2], 3, this.getParam(modes[0], 1) === this.getParam(modes[1], 2) ? 1 : 0);
                    this.pointer += 4;
                    break;
                case 9: // Adjust relative base
                    this.relativeBase += this.getParam(modes[0], 1);
                    this.pointer += 2;
                    break;
                case 99: // Halt
                    return -1;
                default:
                    throw new Error(`Unknown opcode: ${opcode}`);
            }
        }
    }
}

const readInput = (filePath: string): number[] => {
    return fs.readFileSync(filePath, 'utf-8').trim().split(',').map(Number);
};

const main = () => {
    const program = readInput('input.txt');
    const computer = new IntcodeComputer(program);
    const output = computer.run(2);
    console.log(output);
};

main();