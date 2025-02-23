class IntcodeComputer {
    private memory: Map<number, number>;
    private ip: number = 0;
    private relativeBase: number = 0;
    public halted: boolean = false;

    constructor(program: number[]) {
        this.memory = new Map();
        program.forEach((val, i) => this.memory.set(i, val));
    }

    private getParameter(mode: number, offset: number): number {
        const param = this.memory.get(this.ip + offset) || 0;
        switch (mode) {
            case 0: // Position mode
                return this.memory.get(param) || 0;
            case 1: // Immediate mode
                return param;
            case 2: // Relative mode
                return this.memory.get(this.relativeBase + param) || 0;
            default:
                throw new Error(`Unknown parameter mode: ${mode}`);
        }
    }

    private setParameter(mode: number, offset: number, value: number): void {
        const param = this.memory.get(this.ip + offset) || 0;
        let address: number;
        switch (mode) {
            case 0: // Position mode
                address = param;
                break;
            case 2: // Relative mode
                address = this.relativeBase + param;
                break;
            default:
                throw new Error(`Unknown parameter mode for writing: ${mode}`);
        }
        this.memory.set(address, value);
    }

    *run(): Generator<'need_input' | number, void, number | undefined> {
        while (!this.halted) {
            const instruction = this.memory.get(this.ip) || 0;
            const opcode = instruction % 100;
            const modes = [
                Math.floor(instruction / 100) % 10,
                Math.floor(instruction / 1000) % 10,
                Math.floor(instruction / 10000) % 10,
            ];

            switch (opcode) {
                case 1: { // Addition
                    const param1 = this.getParameter(modes[0], 1);
                    const param2 = this.getParameter(modes[1], 2);
                    this.setParameter(modes[2], 3, param1 + param2);
                    this.ip += 4;
                    break;
                }
                case 2: { // Multiplication
                    const param1 = this.getParameter(modes[0], 1);
                    const param2 = this.getParameter(modes[1], 2);
                    this.setParameter(modes[2], 3, param1 * param2);
                    this.ip += 4;
                    break;
                }
                case 3: { // Input
                    const inputVal = yield 'need_input';
                    if (inputVal === undefined) {
                        throw new Error('No input provided');
                    }
                    this.setParameter(modes[0], 1, inputVal);
                    this.ip += 2;
                    break;
                }
                case 4: { // Output
                    const outputVal = this.getParameter(modes[0], 1);
                    this.ip += 2;
                    yield outputVal;
                    break;
                }
                case 5: { // Jump-if-true
                    const param1 = this.getParameter(modes[0], 1);
                    const param2 = this.getParameter(modes[1], 2);
                    this.ip = param1 !== 0 ? param2 : this.ip + 3;
                    break;
                }
                case 6: { // Jump-if-false
                    const param1 = this.getParameter(modes[0], 1);
                    const param2 = this.getParameter(modes[1], 2);
                    this.ip = param1 === 0 ? param2 : this.ip + 3;
                    break;
                }
                case 7: { // Less than
                    const param1 = this.getParameter(modes[0], 1);
                    const param2 = this.getParameter(modes[1], 2);
                    this.setParameter(modes[2], 3, param1 < param2 ? 1 : 0);
                    this.ip += 4;
                    break;
                }
                case 8: { // Equals
                    const param1 = this.getParameter(modes[0], 1);
                    const param2 = this.getParameter(modes[1], 2);
                    this.setParameter(modes[2], 3, param1 === param2 ? 1 : 0);
                    this.ip += 4;
                    break;
                }
                case 9: { // Adjust relative base
                    const param1 = this.getParameter(modes[0], 1);
                    this.relativeBase += param1;
                    this.ip += 2;
                    break;
                }
                case 99: // Halt
                    this.halted = true;
                    return;
                default:
                    throw new Error(`Unknown opcode: ${opcode}`);
            }
        }
    }
}

class Robot {
    private computer: IntcodeComputer;
    private generator: Generator<'need_input' | number, void, number | undefined>;
    private direction: number = 0;
    private position: [number, number] = [0, 0];
    private panels: Map<string, number> = new Map();
    private paintedPanels: Set<string> = new Set();

    constructor(program: number[], startPanelColor: number = 0) {
        this.computer = new IntcodeComputer(program);
        this.generator = this.computer.run();
        this.panels.set(this.getPositionKey(), startPanelColor);
    }

    private getPositionKey(): string {
        return `${this.position[0]},${this.position[1]}`;
    }

    private turnAndMove(turnDirection: number): void {
        if (turnDirection === 0) { // Turn left
            this.direction = (this.direction - 1 + 4) % 4;
        } else if (turnDirection === 1) { // Turn right
            this.direction = (this.direction + 1) % 4;
        } else {
            throw new Error(`Unknown turn direction: ${turnDirection}`);
        }

        const [x, y] = this.position;
        switch (this.direction) {
            case 0: // Up
                this.position = [x, y - 1];
                break;
            case 1: // Right
                this.position = [x + 1, y];
                break;
            case 2: // Down
                this.position = [x, y + 1];
                break;
            case 3: // Left
                this.position = [x - 1, y];
                break;
            default:
                throw new Error(`Invalid direction: ${this.direction}`);
        }
    }

    run(): void {
        let nextResult = this.generator.next();
        while (!nextResult.done) {
            if (nextResult.value === 'need_input') {
                const currentColor = this.panels.get(this.getPositionKey()) || 0;
                nextResult = this.generator.next(currentColor);
            } else {
                const paintColor = nextResult.value;
                const turnResult = this.generator.next();
                if (turnResult.done) break;
                if (turnResult.value === 'need_input') {
                    throw new Error('Unexpected need_input after paint color');
                }
                const turnDirection = turnResult.value;
                this.panels.set(this.getPositionKey(), paintColor);
                this.paintedPanels.add(this.getPositionKey());
                this.turnAndMove(turnDirection);
                nextResult = this.generator.next();
            }
        }
    }

    getPaintedPanelsCount(): number {
        return this.paintedPanels.size;
    }

    renderPanels(): void {
        if (this.panels.size === 0) {
            console.log("No panels painted.");
            return;
        }

        let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
        this.paintedPanels.forEach(posKey => {
            const [x, y] = posKey.split(',').map(Number);
            minX = Math.min(minX, x);
            maxX = Math.max(maxX, x);
            minY = Math.min(minY, y);
            maxY = Math.max(maxY, y);
        });

        const grid: string[] = [];
        for (let y = minY; y <= maxY; y++) {
            let row = '';
            for (let x = minX; x <= maxX; x++) {
                const color = this.panels.get(`${x},${y}`) || 0;
                row += color === 1 ? '#' : ' ';
            }
            grid.push(row);
        }

        console.log("\nRegistration Identifier:");
        grid.forEach(line => console.log(line));
    }
}

function parseInput(filePath: string): number[] {
    const fs = require('fs');
    const content = fs.readFileSync(filePath, 'utf8').trim();
    return content.split(',').map(Number);
}

function main() {
    const inputFile = 'input.txt';
    const program = parseInput(inputFile);

    // Part One
    const robotPart1 = new Robot(program, 0);
    robotPart1.run();
    console.log(`Part One: ${robotPart1.getPaintedPanelsCount()} panels painted at least once.`);

    // Part Two
    const robotPart2 = new Robot(program, 1);
    robotPart2.run();
    console.log("Part Two: Registration identifier painted on the hull.");
    robotPart2.renderPanels();
}

main();
