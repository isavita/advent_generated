const fs = require('fs');

class IntcodeComputer {
    constructor(program) {
        this.memory = [...program];
        this.pointer = 0;
        this.relativeBase = 0;
        this.input = [];
        this.output = [];
        this.halted = false;
    }

    getParam(mode, offset) {
        const param = this.memory[this.pointer + offset] || 0;
        switch (mode) {
            case 0:
                return this.memory[param] || 0;
            case 1:
                return param;
            case 2:
                return this.memory[this.relativeBase + param] || 0;
            default:
                throw new Error(`Unknown parameter mode: ${mode}`);
        }
    }

    setParam(mode, offset, value) {
        const param = this.memory[this.pointer + offset] || 0;
        switch (mode) {
            case 0:
                this.memory[param] = value;
                break;
            case 2:
                this.memory[this.relativeBase + param] = value;
                break;
            default:
                throw new Error(`Unknown parameter mode for writing: ${mode}`);
        }
    }

    run() {
        while (this.pointer < this.memory.length) {
            const instruction = this.memory[this.pointer].toString().padStart(5, '0');
            const opcode = parseInt(instruction.slice(-2));
            const modes = [
                parseInt(instruction[2]),
                parseInt(instruction[1]),
                parseInt(instruction[0]),
            ];

            switch (opcode) {
                case 1: { // Add
                    const a = this.getParam(modes[0], 1);
                    const b = this.getParam(modes[1], 2);
                    this.setParam(modes[2], 3, a + b);
                    this.pointer += 4;
                    break;
                }
                case 2: { // Multiply
                    const a = this.getParam(modes[0], 1);
                    const b = this.getParam(modes[1], 2);
                    this.setParam(modes[2], 3, a * b);
                    this.pointer += 4;
                    break;
                }
                case 3: { // Input
                    if (this.input.length === 0) {
                        return; // Awaiting input
                    }
                    const inputValue = this.input.shift();
                    this.setParam(modes[0], 1, inputValue);
                    this.pointer += 2;
                    break;
                }
                case 4: { // Output
                    const outputValue = this.getParam(modes[0], 1);
                    this.output.push(outputValue);
                    this.pointer += 2;
                    break;
                }
                case 5: { // Jump-if-true
                    const a = this.getParam(modes[0], 1);
                    const b = this.getParam(modes[1], 2);
                    if (a !== 0) {
                        this.pointer = b;
                    } else {
                        this.pointer += 3;
                    }
                    break;
                }
                case 6: { // Jump-if-false
                    const a = this.getParam(modes[0], 1);
                    const b = this.getParam(modes[1], 2);
                    if (a === 0) {
                        this.pointer = b;
                    } else {
                        this.pointer += 3;
                    }
                    break;
                }
                case 7: { // Less than
                    const a = this.getParam(modes[0], 1);
                    const b = this.getParam(modes[1], 2);
                    this.setParam(modes[2], 3, a < b ? 1 : 0);
                    this.pointer += 4;
                    break;
                }
                case 8: { // Equals
                    const a = this.getParam(modes[0], 1);
                    const b = this.getParam(modes[1], 2);
                    this.setParam(modes[2], 3, a === b ? 1 : 0);
                    this.pointer += 4;
                    break;
                }
                case 9: { // Adjust relative base
                    const a = this.getParam(modes[0], 1);
                    this.relativeBase += a;
                    this.pointer += 2;
                    break;
                }
                case 99: { // Halt
                    this.halted = true;
                    return;
                }
                default:
                    throw new Error(`Unknown opcode: ${opcode} at position ${this.pointer}`);
            }
        }
    }
}

function readProgram(filename) {
    const data = fs.readFileSync(filename, 'utf-8').trim();
    return data.split(',').map(Number);
}

function main() {
    const program = readProgram('input.txt');
    program[0] = 2; // Insert two quarters to play for free
    const computer = new IntcodeComputer(program);

    const screen = {};
    let score = 0;
    let paddleX = 0;
    let ballX = 0;

    while (!computer.halted) {
        computer.run();

        while (computer.output.length >= 3) {
            const x = computer.output.shift();
            const y = computer.output.shift();
            const tile = computer.output.shift();

            if (x === -1 && y === 0) {
                score = tile;
            } else {
                screen[`${x},${y}`] = tile;
                if (tile === 3) { // Paddle
                    paddleX = x;
                } else if (tile === 4) { // Ball
                    ballX = x;
                }
            }
        }

        // Determine joystick input
        let joystick = 0;
        if (ballX < paddleX) {
            joystick = -1;
        } else if (ballX > paddleX) {
            joystick = 1;
        }
        computer.input.push(joystick);
    }

    console.log(`Final Score: ${score}`);
}

main();
