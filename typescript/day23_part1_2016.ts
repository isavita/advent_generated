type Instruction = {
    op: string;
    args: (string | number)[];
};

class AssembunnyInterpreter {
    private instructions: Instruction[];
    private registers: { [key: string]: number };
    private ip: number;

    constructor(instructions: Instruction[]) {
        this.instructions = instructions;
        this.registers = {};
        this.ip = 0;
    }

    private getValue(arg: string | number): number {
        if (typeof arg === 'number') {
            return arg;
        }
        return this.registers[arg] || 0;
    }

    private toggleInstruction(index: number): void {
        if (index < 0 || index >= this.instructions.length) {
            return;
        }

        const instr = this.instructions[index];
        switch (instr.op) {
            case 'inc':
                instr.op = 'dec';
                break;
            case 'dec':
            case 'tgl':
            case 'out':
                instr.op = 'inc';
                break;
            case 'jnz':
                instr.op = 'cpy';
                break;
            case 'cpy':
                instr.op = 'jnz';
                break;
        }
    }

    public run(): void {
        while (this.ip < this.instructions.length) {
            const instr = this.instructions[this.ip];
            switch (instr.op) {
                case 'cpy':
                    if (typeof instr.args[1] === 'string') {
                        this.registers[instr.args[1]] = this.getValue(instr.args[0]);
                    }
                    break;
                case 'inc':
                    if (typeof instr.args[0] === 'string') {
                        this.registers[instr.args[0]] = (this.registers[instr.args[0]] || 0) + 1;
                    }
                    break;
                case 'dec':
                    if (typeof instr.args[0] === 'string') {
                        this.registers[instr.args[0]] = (this.registers[instr.args[0]] || 0) - 1;
                    }
                    break;
                case 'jnz':
                    if (this.getValue(instr.args[0]) !== 0) {
                        this.ip += this.getValue(instr.args[1]) - 1;
                    }
                    break;
                case 'tgl':
                    this.toggleInstruction(this.ip + this.getValue(instr.args[0]));
                    break;
            }
            this.ip++;
        }
    }

    public getRegister(name: string): number {
        return this.registers[name] || 0;
    }

    public setRegister(name: string, value: number): void {
        this.registers[name] = value;
    }
}

function parseInstructions(input: string): Instruction[] {
    return input.split('\n').map(line => {
        const parts = line.split(' ');
        const op = parts[0];
        const args = parts.slice(1).map(arg => isNaN(Number(arg)) ? arg : Number(arg));
        return { op, args };
    });
}

// Read input from file
const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8');

// Parse instructions
const instructions = parseInstructions(input);

// Initialize interpreter with register 'a' set to 7
const interpreter = new AssembunnyInterpreter(instructions);
interpreter.setRegister('a', 7);

// Run the program
interpreter.run();

// Print the value of register 'a'
console.log(interpreter.getRegister('a'));