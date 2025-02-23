
import * as fs from 'fs';

class IntcodeComputer {
    memory: Map<number, number>;
    ip: number;
    relativeBase: number;
    inputs: number[];
    outputs: number[];
    halted: boolean;
    needsInput: boolean;

    constructor(program: number[], inputs: number[] = []) {
        this.memory = new Map(program.map((value, index) => [index, value]));
        this.ip = 0;
        this.relativeBase = 0;
        this.inputs = [...inputs];
        this.outputs = [];
        this.halted = false;
        this.needsInput = false;
    }

    getParam(mode: number, offset: number): number {
        const address = this.ip + offset;
        switch (mode) {
            case 0:
                return this.memory.get(this.memory.get(address) || 0) || 0;
            case 1:
                return this.memory.get(address) || 0;
            case 2:
                return this.memory.get(this.relativeBase + (this.memory.get(address) || 0)) || 0;
            default:
                throw new Error(`Unknown mode ${mode}`);
        }
    }

    setParam(mode: number, offset: number, value: number): void {
        let address: number;
        switch (mode) {
            case 0:
                address = this.memory.get(this.ip + offset) || 0;
                break;
            case 2:
                address = this.relativeBase + (this.memory.get(this.ip + offset) || 0);
                break;
            default:
                throw new Error(`Unknown mode ${mode}`);
        }
        this.memory.set(address, value);
    }

    run(): void {
        while (true) {
            const opcodeFull = this.memory.get(this.ip) || 0;
            const opcode = opcodeFull % 100;
            const modes = [
                Math.floor(opcodeFull / 100) % 10,
                Math.floor(opcodeFull / 1000) % 10,
                Math.floor(opcodeFull / 10000) % 10,
            ];

            if (opcode === 99) {
                this.halted = true;
                break;
            } else if ([1, 2, 7, 8].includes(opcode)) {
                const param1 = this.getParam(modes[0], 1);
                const param2 = this.getParam(modes[1], 2);
                let result: number;
                if (opcode === 1) {
                    result = param1 + param2;
                } else if (opcode === 2) {
                    result = param1 * param2;
                } else if (opcode === 7) {
                    result = param1 < param2 ? 1 : 0;
                } else if (opcode === 8) {
                    result = param1 === param2 ? 1 : 0;
                } else {
                    throw new Error(`Unexpected opcode ${opcode}`);
                }
                this.setParam(modes[2], 3, result);
                this.ip += 4;
            } else if (opcode === 3) {
                if (this.inputs.length === 0) {
                    this.needsInput = true;
                    return;
                }
                this.needsInput = false;
                const value = this.inputs.shift()!;
                this.setParam(modes[0], 1, value);
                this.ip += 2;
            } else if (opcode === 4) {
                const param1 = this.getParam(modes[0], 1);
                this.outputs.push(param1);
                this.ip += 2;
                if (this.outputs.length === 3) {
                    return;
                }
            } else if ([5, 6].includes(opcode)) {
                const param1 = this.getParam(modes[0], 1);
                const param2 = this.getParam(modes[1], 2);
                if ((opcode === 5 && param1 !== 0) || (opcode === 6 && param1 === 0)) {
                    this.ip = param2;
                } else {
                    this.ip += 3;
                }
            } else if (opcode === 9) {
                const param1 = this.getParam(modes[0], 1);
                this.relativeBase += param1;
                this.ip += 2;
            } else {
                throw new Error(`Unknown opcode ${opcode}`);
            }
        }
    }
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const program = input.split(',').map(Number);

    const computers: IntcodeComputer[] = [];
    const packetQueues: number[][] = Array(50).fill(null).map(() => []);

    for (let i = 0; i < 50; i++) {
        computers.push(new IntcodeComputer([...program], [i]));
    }

    let firstPacketTo255: number | null = null;

    while (true) {
        let idle = true;
        for (let i = 0; i < 50; i++) {
            const computer = computers[i];
            if (packetQueues[i].length > 0) {
                const x = packetQueues[i].shift()!;
                const y = packetQueues[i].shift()!;
                computer.inputs.push(x, y);
            } else {
                computer.inputs.push(-1);
            }

            computer.run();

            while (computer.outputs.length >= 3) {
                idle = false;
                const dest = computer.outputs.shift()!;
                const x = computer.outputs.shift()!;
                const y = computer.outputs.shift()!;

                if (dest === 255) {
                    if (firstPacketTo255 === null) {
                        firstPacketTo255 = y;
                        console.log(firstPacketTo255);
                        return;
                    }
                } else if (dest >= 0 && dest < 50) {
                    packetQueues[dest].push(x, y);
                }
            }
        }
    }
}

main();
