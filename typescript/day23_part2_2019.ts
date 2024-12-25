
import * as fs from 'fs';

type Instruction = {
    opcode: number;
    modes: number[];
};

type Packet = {
    x: number;
    y: number;
};

class IntcodeComputer {
    memory: number[];
    pc: number;
    relativeBase: number;
    inputQueue: number[];
    outputQueue: number[];
    halted: boolean;
    waitingForInput: boolean;

    constructor(program: number[]) {
        this.memory = [...program];
        this.pc = 0;
        this.relativeBase = 0;
        this.inputQueue = [];
        this.outputQueue = [];
        this.halted = false;
        this.waitingForInput = false;
    }

    private getParam(mode: number, offset: number): number {
        const address = this.pc + offset;
        switch (mode) {
            case 0: // Position mode
                return this.memory[address] || 0;
            case 1: // Immediate mode
                return address;
            case 2: // Relative mode
                return (this.memory[address] || 0) + this.relativeBase;
            default:
                throw new Error(`Invalid mode: ${mode}`);
        }
    }

    private readValue(mode: number, offset: number): number {
        const address = this.getParam(mode, offset);
        return this.memory[address] || 0;
    }

    private writeValue(mode: number, offset: number, value: number): void {
        const address = this.getParam(mode, offset);
        this.memory[address] = value;
    }

    private parseInstruction(): Instruction {
        const instruction = this.memory[this.pc] || 0;
        const opcode = instruction % 100;
        const modes = [
            Math.floor(instruction / 100) % 10,
            Math.floor(instruction / 1000) % 10,
            Math.floor(instruction / 10000) % 10,
        ];
        return { opcode, modes };
    }

    run(): void {
        this.waitingForInput = false;
        while (!this.halted && !this.waitingForInput) {
            const { opcode, modes } = this.parseInstruction();

            switch (opcode) {
                case 1: // Add
                    this.writeValue(modes[2], 3, this.readValue(modes[0], 1) + this.readValue(modes[1], 2));
                    this.pc += 4;
                    break;
                case 2: // Multiply
                    this.writeValue(modes[2], 3, this.readValue(modes[0], 1) * this.readValue(modes[1], 2));
                    this.pc += 4;
                    break;
                case 3: // Input
                    if (this.inputQueue.length === 0) {
                        this.waitingForInput = true;
                        return;
                    }
                    this.writeValue(modes[0], 1, this.inputQueue.shift()!);
                    this.pc += 2;
                    break;
                case 4: // Output
                    this.outputQueue.push(this.readValue(modes[0], 1));
                    this.pc += 2;
                    break;
                case 5: // Jump-if-true
                    if (this.readValue(modes[0], 1) !== 0) {
                        this.pc = this.readValue(modes[1], 2);
                    } else {
                        this.pc += 3;
                    }
                    break;
                case 6: // Jump-if-false
                    if (this.readValue(modes[0], 1) === 0) {
                        this.pc = this.readValue(modes[1], 2);
                    } else {
                        this.pc += 3;
                    }
                    break;
                case 7: // Less than
                    this.writeValue(modes[2], 3, this.readValue(modes[0], 1) < this.readValue(modes[1], 2) ? 1 : 0);
                    this.pc += 4;
                    break;
                case 8: // Equals
                    this.writeValue(modes[2], 3, this.readValue(modes[0], 1) === this.readValue(modes[1], 2) ? 1 : 0);
                    this.pc += 4;
                    break;
                case 9: // Adjust relative base
                    this.relativeBase += this.readValue(modes[0], 1);
                    this.pc += 2;
                    break;
                case 99: // Halt
                    this.halted = true;
                    break;
                default:
                    throw new Error(`Invalid opcode: ${opcode}`);
            }
        }
    }
}

function solve(program: number[]): { part1: number, part2: number } {
    const computers: IntcodeComputer[] = [];
    const packetQueues: number[][] = Array(50).fill(null).map(() => []);
    let natPacket: Packet | null = null;
    let lastNatY: number | null = null;
    let firstRepeatedNatY: number | null = null;
    let idleCounter = 0;

    for (let i = 0; i < 50; i++) {
        const computer = new IntcodeComputer(program);
        computer.inputQueue.push(i);
        computers.push(computer);
    }

    let part1Answer: number | null = null;

    while (firstRepeatedNatY === null) {
        let networkIdle = true;
        for (let i = 0; i < 50; i++) {
            const computer = computers[i];
            computer.run();

            while (computer.outputQueue.length >= 3) {
                const dest = computer.outputQueue.shift()!;
                const x = computer.outputQueue.shift()!;
                const y = computer.outputQueue.shift()!;

                if (dest === 255) {
                    natPacket = { x, y };
                    if (part1Answer === null) {
                        part1Answer = y;
                    }
                } else {
                    packetQueues[dest].push(x);
                    packetQueues[dest].push(y);
                }
            }

            if (computer.waitingForInput && packetQueues[i].length > 0) {
                computer.inputQueue.push(packetQueues[i].shift()!);
                computer.inputQueue.push(packetQueues[i].shift()!);
                networkIdle = false;
            } else if (computer.waitingForInput) {
                computer.inputQueue.push(-1);
            } else {
                networkIdle = false;
            }
        }

        if (networkIdle) {
            idleCounter++;
            if (idleCounter > 10 && natPacket) {
                if (lastNatY === natPacket.y) {
                    firstRepeatedNatY = natPacket.y;
                }
                lastNatY = natPacket.y;
                packetQueues[0].push(natPacket.x);
                packetQueues[0].push(natPacket.y);
                idleCounter = 0;
            }
        } else {
            idleCounter = 0;
        }
    }

    return { part1: part1Answer!, part2: firstRepeatedNatY! };
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const program = input.split(',').map(Number);

    const { part1, part2 } = solve(program);
    console.log(`Part 1: ${part1}`);
    console.log(`Part 2: ${part2}`);
}

main();
