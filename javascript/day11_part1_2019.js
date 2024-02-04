const fs = require('fs');

const PanelColor = {
    Black: 0,
    White: 1
};

const Direction = {
    Up: 0,
    Right: 1,
    Down: 2,
    Left: 3
};

class Robot {
    constructor() {
        this.x = 0;
        this.y = 0;
        this.direction = Direction.Up;
    }

    turnAndMove(turnDirection) {
        if (turnDirection === 0) {
            this.direction = (this.direction + 3) % 4; // Turn left
        } else {
            this.direction = (this.direction + 1) % 4; // Turn right
        }

        switch (this.direction) {
            case Direction.Up:
                this.y--;
                break;
            case Direction.Right:
                this.x++;
                break;
            case Direction.Down:
                this.y++;
                break;
            case Direction.Left:
                this.x--;
                break;
        }
    }
}

class Intcode {
    constructor(program) {
        this.memory = [...program];
        this.ip = 0;
        this.input = [];
        this.output = [];
        this.halted = false;
    }

    addInput(input) {
        this.input.push(input);
    }

    run() {
        this.output = [];
        while (!this.halted) {
            const opcode = this.memory[this.ip] % 100;
            switch (opcode) {
                case 1:
                case 2:
                case 7:
                case 8:
                    this.ensureMemory(this.ip + 3);
                    const params = this.getParams(3);
                    const val1 = this.readMemory(params[0]);
                    const val2 = this.readMemory(params[1]);
                    if (opcode === 1) {
                        this.writeMemory(params[2], val1 + val2);
                    } else if (opcode === 2) {
                        this.writeMemory(params[2], val1 * val2);
                    } else if ((opcode === 7 && val1 < val2) || (opcode === 8 && val1 === val2)) {
                        this.writeMemory(params[2], 1);
                    } else {
                        this.writeMemory(params[2], 0);
                    }
                    this.ip += 4;
                    break;
                case 3:
                case 4:
                    this.ensureMemory(this.ip + 1);
                    const paramsIO = this.getParams(1);
                    if (opcode === 3) {
                        if (this.input.length === 0) {
                            return; // Wait for more input
                        }
                        this.writeMemory(paramsIO[0], this.input.shift());
                    } else {
                        this.output.push(this.readMemory(paramsIO[0]));
                    }
                    this.ip += 2;
                    break;
                case 5:
                case 6:
                    this.ensureMemory(this.ip + 2);
                    const paramsJump = this.getParams(2);
                    const val = this.readMemory(paramsJump[0]);
                    const target = this.readMemory(paramsJump[1]);
                    if ((opcode === 5 && val !== 0) || (opcode === 6 && val === 0)) {
                        this.ip = target;
                    } else {
                        this.ip += 3;
                    }
                    break;
                case 99:
                    this.halted = true;
                    break;
                default:
                    throw new Error(`Unknown opcode: ${opcode}`);
            }
        }
    }

    readMemory(address) {
        this.ensureMemory(address);
        return this.memory[address];
    }

    writeMemory(address, value) {
        this.ensureMemory(address);
        this.memory[address] = value;
    }

    ensureMemory(address) {
        if (address >= this.memory.length) {
            const newMemory = new Array(address + 1).fill(0);
            this.memory = [...this.memory, ...newMemory.slice(this.memory.length)];
        }
    }

    getParams(count) {
        let paramModes = Math.floor(this.memory[this.ip] / 100);
        const params = [];
        for (let i = 0; i < count; i++) {
            params[i] = paramModes % 10 === 1 ? this.ip + i + 1 : this.memory[this.ip + i + 1];
            paramModes = Math.floor(paramModes / 10);
        }
        return params;
    }

    getOutputs() {
        return this.output;
    }

    isHalted() {
        return this.halted;
    }
}

const data = fs.readFileSync('input.txt', 'utf8');
const codeStr = data.trim().split(',');
const program = codeStr.map(Number);

const grid = new Map();
const robot = new Robot();
const intcode = new Intcode(program);

while (!intcode.isHalted()) {
    const currentColor = grid.get(`${robot.x},${robot.y}`) || PanelColor.Black;
    intcode.addInput(currentColor);
    intcode.run();
    const outputs = intcode.getOutputs();

    if (outputs.length === 2) {
        grid.set(`${robot.x},${robot.y}`, outputs[0]);
        robot.turnAndMove(outputs[1]);
    }
}

console.log(grid.size);