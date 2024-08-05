const fs = require('fs');
const readline = require('readline');

class IntcodeComputer {
    constructor(program, address) {
        this.program = [...program];
        this.address = address;
        this.pointer = 0;
        this.relativeBase = 0;
        this.inputs = [address];
        this.outputs = [];
        this.halted = false;
    }

    getParam(mode, param) {
        if (mode === 0) return this.program[param];
        if (mode === 1) return param;
        if (mode === 2) return this.program[this.relativeBase + param];
        throw new Error(`Invalid mode ${mode}`);
    }

    setParam(mode, param, value) {
        const index = mode === 2 ? this.relativeBase + param : param;
        this.program[index] = value;
    }

    run() {
        while (this.pointer < this.program.length) {
            const instruction = this.program[this.pointer].toString().padStart(5, '0');
            const opcode = parseInt(instruction.slice(-2), 10);
            const modes = instruction.slice(0, 3).split('').map(Number).reverse();

            if (opcode === 99) {
                this.halted = true;
                break;
            }

            const params = [1, 2, 3].map(i => this.program[this.pointer + i]);

            if (opcode === 1) {
                this.setParam(modes[2], params[2], this.getParam(modes[0], params[0]) + this.getParam(modes[1], params[1]));
                this.pointer += 4;
            } else if (opcode === 2) {
                this.setParam(modes[2], params[2], this.getParam(modes[0], params[0]) * this.getParam(modes[1], params[1]));
                this.pointer += 4;
            } else if (opcode === 3) {
                if (this.inputs.length === 0) break;
                this.setParam(modes[0], params[0], this.inputs.shift());
                this.pointer += 2;
            } else if (opcode === 4) {
                this.outputs.push(this.getParam(modes[0], params[0]));
                this.pointer += 2;
                if (this.outputs.length === 3) return;
            } else if (opcode === 5) {
                if (this.getParam(modes[0], params[0]) !== 0) {
                    this.pointer = this.getParam(modes[1], params[1]);
                } else {
                    this.pointer += 3;
                }
            } else if (opcode === 6) {
                if (this.getParam(modes[0], params[0]) === 0) {
                    this.pointer = this.getParam(modes[1], params[1]);
                } else {
                    this.pointer += 3;
                }
            } else if (opcode === 7) {
                this.setParam(modes[2], params[2], this.getParam(modes[0], params[0]) < this.getParam(modes[1], params[1]) ? 1 : 0);
                this.pointer += 4;
            } else if (opcode === 8) {
                this.setParam(modes[2], params[2], this.getParam(modes[0], params[0]) === this.getParam(modes[1], params[1]) ? 1 : 0);
                this.pointer += 4;
            } else if (opcode === 9) {
                this.relativeBase += this.getParam(modes[0], params[0]);
                this.pointer += 2;
            } else {
                throw new Error(`Unknown opcode ${opcode}`);
            }
        }
    }
}

async function readInput(filePath) {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let program = [];
    for await (const line of rl) {
        program = line.split(',').map(Number);
    }
    return program;
}

async function main() {
    const program = await readInput('input.txt');
    const computers = Array.from({ length: 50 }, (_, i) => new IntcodeComputer(program, i));
    const packets = Array.from({ length: 50 }, () => []);
    let natPacket = null;

    while (true) {
        for (const computer of computers) {
            computer.run();
            if (computer.outputs.length === 3) {
                const [dest, x, y] = computer.outputs;
                computer.outputs = [];
                if (dest === 255) {
                    console.log(`First packet to address 255: Y = ${y}`);
                    return;
                }
                packets[dest].push([x, y]);
            }
        }

        for (const computer of computers) {
            if (packets[computer.address].length > 0) {
                const [x, y] = packets[computer.address].shift();
                computer.inputs.push(x, y);
            } else {
                computer.inputs.push(-1);
            }
        }
    }
}

main().catch(console.error);