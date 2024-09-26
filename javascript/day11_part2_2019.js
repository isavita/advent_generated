const fs = require('fs');
const path = require('path');

// Intcode Computer Class
class IntcodeComputer {
    constructor(program) {
        this.memory = program.slice();
        this.pointer = 0;
        this.relativeBase = 0;
        this.input = [];
        this.output = [];
        this.halted = false;
    }

    getParam(mode, param) {
        switch(mode) {
            case 0: // position mode
                if (param >= this.memory.length) this.memory[param] = 0;
                return this.memory[param] || 0;
            case 1: // immediate mode
                return param;
            case 2: // relative mode
                const addr = this.relativeBase + param;
                if (addr >= this.memory.length) this.memory[addr] = 0;
                return this.memory[addr] || 0;
            default:
                throw new Error(`Unknown parameter mode: ${mode}`);
        }
    }

    setParam(mode, param, value) {
        let addr;
        if (mode === 0) { // position mode
            addr = param;
        } else if (mode === 2) { // relative mode
            addr = this.relativeBase + param;
        } else {
            throw new Error(`Unsupported mode for writing: ${mode}`);
        }
        if (addr >= this.memory.length) {
            this.memory.length = addr + 1;
        }
        this.memory[addr] = value;
    }

    run() {
        while (true) {
            let instruction = this.memory[this.pointer].toString().padStart(5, '0');
            let opcode = parseInt(instruction.slice(-2));
            let modes = instruction.slice(0, 3).split('').reverse().map(Number);

            if (opcode === 99) {
                this.halted = true;
                break;
            }

            let param1 = this.memory[this.pointer + 1];
            let param2 = this.memory[this.pointer + 2];
            let param3 = this.memory[this.pointer + 3];

            switch(opcode) {
                case 1: { // add
                    let val1 = this.getParam(modes[0], param1);
                    let val2 = this.getParam(modes[1], param2);
                    this.setParam(modes[2], param3, val1 + val2);
                    this.pointer += 4;
                    break;
                }
                case 2: { // multiply
                    let val1 = this.getParam(modes[0], param1);
                    let val2 = this.getParam(modes[1], param2);
                    this.setParam(modes[2], param3, val1 * val2);
                    this.pointer += 4;
                    break;
                }
                case 3: { // input
                    if (this.input.length === 0) {
                        return; // wait for input
                    }
                    let inputVal = this.input.shift();
                    this.setParam(modes[0], param1, inputVal);
                    this.pointer += 2;
                    break;
                }
                case 4: { // output
                    let outputVal = this.getParam(modes[0], param1);
                    this.output.push(outputVal);
                    this.pointer += 2;
                    break;
                }
                case 5: { // jump-if-true
                    let val1 = this.getParam(modes[0], param1);
                    let val2 = this.getParam(modes[1], param2);
                    if (val1 !== 0) {
                        this.pointer = val2;
                    } else {
                        this.pointer += 3;
                    }
                    break;
                }
                case 6: { // jump-if-false
                    let val1 = this.getParam(modes[0], param1);
                    let val2 = this.getParam(modes[1], param2);
                    if (val1 === 0) {
                        this.pointer = val2;
                    } else {
                        this.pointer += 3;
                    }
                    break;
                }
                case 7: { // less than
                    let val1 = this.getParam(modes[0], param1);
                    let val2 = this.getParam(modes[1], param2);
                    this.setParam(modes[2], param3, val1 < val2 ? 1 : 0);
                    this.pointer += 4;
                    break;
                }
                case 8: { // equals
                    let val1 = this.getParam(modes[0], param1);
                    let val2 = this.getParam(modes[1], param2);
                    this.setParam(modes[2], param3, val1 === val2 ? 1 : 0);
                    this.pointer += 4;
                    break;
                }
                case 9: { // adjust relative base
                    let val1 = this.getParam(modes[0], param1);
                    this.relativeBase += val1;
                    this.pointer += 2;
                    break;
                }
                default:
                    throw new Error(`Unknown opcode: ${opcode} at position ${this.pointer}`);
            }
        }
    }

    provideInput(inputValue) {
        this.input.push(inputValue);
    }

    getOutput() {
        const out = this.output.slice();
        this.output = [];
        return out;
    }
}

// Robot Class
class Robot {
    constructor(initialPanel = 0) {
        this.x = 0;
        this.y = 0;
        this.direction = 0; // 0: up, 1: right, 2: down, 3: left
        this.panels = new Map();
        if (initialPanel !== undefined) {
            this.panels.set('0,0', initialPanel);
        }
        this.paintedPanels = new Set();
    }

    getCurrentPanelColor() {
        const key = `${this.x},${this.y}`;
        return this.panels.get(key) || 0;
    }

    paintPanel(color) {
        const key = `${this.x},${this.y}`;
        this.panels.set(key, color);
        this.paintedPanels.add(key);
    }

    turn(turnDirection) {
        if (turnDirection === 0) { // left
            this.direction = (this.direction + 3) % 4;
        } else if (turnDirection === 1) { // right
            this.direction = (this.direction + 1) % 4;
        } else {
            throw new Error(`Unknown turn direction: ${turnDirection}`);
        }
    }

    moveForward() {
        switch(this.direction) {
            case 0: this.y += 1; break; // up
            case 1: this.x += 1; break; // right
            case 2: this.y -= 1; break; // down
            case 3: this.x -= 1; break; // left
            default: throw new Error(`Unknown direction: ${this.direction}`);
        }
    }
}

// Main function
function main() {
    const inputPath = path.join(__dirname, 'input.txt');
    const program = fs.readFileSync(inputPath, 'utf8').trim().split(',').map(Number);

    // Part One
    const robot1 = new Robot();
    const computer1 = new IntcodeComputer(program);
    
    while (!computer1.halted) {
        const currentColor = robot1.getCurrentPanelColor();
        computer1.provideInput(currentColor);
        computer1.run();
        const outputs = computer1.getOutput();
        if (outputs.length >= 2) {
            const paintColor = outputs[0];
            const turnDirection = outputs[1];
            robot1.paintPanel(paintColor);
            robot1.turn(turnDirection);
            robot1.moveForward();
        }
    }

    console.log(`Part One: Number of panels painted at least once: ${robot1.paintedPanels.size}`);

    // Part Two
    const robot2 = new Robot(1); // starting panel is white
    const computer2 = new IntcodeComputer(program);
    
    while (!computer2.halted) {
        const currentColor = robot2.getCurrentPanelColor();
        computer2.provideInput(currentColor);
        computer2.run();
        const outputs = computer2.getOutput();
        if (outputs.length >= 2) {
            const paintColor = outputs[0];
            const turnDirection = outputs[1];
            robot2.paintPanel(paintColor);
            robot2.turn(turnDirection);
            robot2.moveForward();
        }
    }

    // Find bounds
    const paintedKeys = robot2.panels.keys();
    let minX = 0, maxX = 0, minY = 0, maxY = 0;
    for (let key of paintedKeys) {
        const [x, y] = key.split(',').map(Number);
        if (x < minX) minX = x;
        if (x > maxX) maxX = x;
        if (y < minY) minY = y;
        if (y > maxY) maxY = y;
    }

    // Print the registration identifier
    const grid = [];
    for (let y = maxY; y >= minY; y--) {
        let row = '';
        for (let x = minX; x <= maxX; x++) {
            const color = robot2.panels.get(`${x},${y}`) || 0;
            row += color === 1 ? '#' : ' ';
        }
        grid.push(row);
    }

    console.log(`Part Two: Registration Identifier:`);
    for (let row of grid) {
        console.log(row);
    }
}

main();
