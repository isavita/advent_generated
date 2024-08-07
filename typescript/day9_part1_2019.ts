import * as fs from 'fs';

class IntcodeComputer {
    private memory: number[];
    private pointer: number;
    private relativeBase: number;

    constructor(program: number[]) {
        this.memory = [...program];
        this.pointer = 0;
        this.relativeBase = 0;
        this.memory.length = Math.max(this.memory.length, 10000); // Ensure sufficient memory
    }

    private getValue(param: number, mode: number): number {
        switch (mode) {
            case 0: return this.memory[param] || 0; // Position mode
            case 1: return param; // Immediate mode
            case 2: return this.memory[this.relativeBase + param] || 0; // Relative mode
            default: throw new Error(`Unknown mode: ${mode}`);
        }
    }

    private setValue(param: number, mode: number, value: number): void {
        switch (mode) {
            case 0: this.memory[param] = value; break; // Position mode
            case 2: this.memory[this.relativeBase + param] = value; break; // Relative mode
            default: throw new Error(`Unknown mode: ${mode}`);
        }
    }

    public run(input: number): number[] {
        const output: number[] = [];
        while (true) {
            const instruction = this.memory[this.pointer].toString().padStart(5, '0');
            const opcode = parseInt(instruction.slice(-2));
            const modes = instruction.slice(0, -2).split('').reverse().map(Number);

            const params = this.memory.slice(this.pointer + 1, this.pointer + 4);
            const values = params.map((param, index) => this.getValue(param, modes[index] || 0));

            switch (opcode) {
                case 1: this.setValue(params[2], modes[2], values[0] + values[1]); this.pointer += 4; break;
                case 2: this.setValue(params[2], modes[2], values[0] * values[1]); this.pointer += 4; break;
                case 3: this.setValue(params[0], modes[0], input); this.pointer += 2; break;
                case 4: output.push(values[0]); this.pointer += 2; break;
                case 5: this.pointer = values[0] !== 0 ? values[1] : this.pointer + 3; break;
                case 6: this.pointer = values[0] === 0 ? values[1] : this.pointer + 3; break;
                case 7: this.setValue(params[2], modes[2], values[0] < values[1] ? 1 : 0); this.pointer += 4; break;
                case 8: this.setValue(params[2], modes[2], values[0] === values[1] ? 1 : 0); this.pointer += 4; break;
                case 9: this.relativeBase += values[0]; this.pointer += 2; break;
                case 99: return output;
                default: throw new Error(`Unknown opcode: ${opcode}`);
            }
        }
    }
}

const main = async () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split(',').map(Number);
    const computer = new IntcodeComputer(input);
    const output = computer.run(1);
    console.log(output[0]);
};

main().catch(console.error);