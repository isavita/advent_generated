
import * as fs from 'fs';

class IntcodeComputer {
    memory: { [key: number]: number } = {};
    ip: number = 0;
    relativeBase: number = 0;
    halted: boolean = false;

    constructor(program: number[]) {
        for (let i = 0; i < program.length; i++) {
            this.memory[i] = program[i];
        }
    }

    getParameter(mode: number, offset: number): number {
        const param = this.memory[this.ip + offset] || 0;
        switch (mode) {
            case 0: return this.memory[param] || 0;
            case 1: return param;
            case 2: return this.memory[this.relativeBase + param] || 0;
            default: throw new Error(`Unknown parameter mode: ${mode}`);
        }
    }

    setParameter(mode: number, offset: number, value: number): void {
        const param = this.memory[this.ip + offset] || 0;
        switch (mode) {
            case 0: this.memory[param] = value; break;
            case 2: this.memory[this.relativeBase + param] = value; break;
            default: throw new Error(`Unknown parameter mode for writing: ${mode}`);
        }
    }

    *run(): Generator<number | 'need_input', void, number> {
        while (true) {
            const instruction = this.memory[this.ip] || 0;
            const opcode = instruction % 100;
            const modes = [
                Math.floor((instruction / 100) % 10),
                Math.floor((instruction / 1000) % 10),
                Math.floor((instruction / 10000) % 10)
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
                    if (param1 !== 0) {
                        this.ip = param2;
                    } else {
                        this.ip += 3;
                    }
                    break;
                }
                case 6: { // Jump-if-false
                    const param1 = this.getParameter(modes[0], 1);
                    const param2 = this.getParameter(modes[1], 2);
                    if (param1 === 0) {
                        this.ip = param2;
                    } else {
                        this.ip += 3;
                    }
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
                case 99: { // Halt
                    this.halted = true;
                    return;
                }
                default:
                    throw new Error(`Unknown opcode: ${opcode}`);
            }
        }
    }
}

class Droid {
    computer: IntcodeComputer;
    generator: Generator<number | 'need_input', void, number>;
    directionMap: { [key: number]: [number, number] } = {
        1: [0, -1],  // North
        2: [0, 1],   // South
        3: [-1, 0],  // West
        4: [1, 0]    // East
    };
    currentPosition: [number, number] = [0, 0];
    grid: { [key: string]: number } = {}; // "(x,y)" -> status (0: wall, 1: open, 2: oxygen)
    oxygenPosition: [number, number] | null = null;
    pathLength: number = 0;

    constructor(program: number[]) {
        this.computer = new IntcodeComputer(program);
        this.generator = this.computer.run();
        this.grid[this.getKey(this.currentPosition)] = 1; // Starting position is open
    }

    private getKey(pos: [number, number]): string {
        return `${pos[0]},${pos[1]}`;
    }

    sendMoveCommand(direction: number): number {
        let status = this.generator.next();
        if (status.value !== 'need_input') {
            throw new Error(`Expected 'need_input' but got ${status.value}`);
        }

        status = this.generator.next(direction);
        if (status.value !== 0 && status.value !== 1 && status.value !== 2) {
            throw new Error(`Unexpected status code: ${status.value}`);
        }
        return status.value;
    }

    explore(): number {
        const queue: [[number, number], number][] = [[this.currentPosition, 0]];
        const visited = new Set<string>([this.getKey(this.currentPosition)]);

        while (queue.length > 0) {
            const [position, steps] = queue.shift()!;
            this.moveTo(position);

            for (let direction = 1; direction <= 4; direction++) {
                const [dx, dy] = this.directionMap[direction];
                const newPos: [number, number] = [position[0] + dx, position[1] + dy];
                const newPosKey = this.getKey(newPos);

                if (visited.has(newPosKey)) {
                    continue;
                }

                const status = this.sendMoveCommand(direction);

                if (status === 0) {
                    this.grid[newPosKey] = 0;
                } else {
                    this.grid[newPosKey] = status;
                    visited.add(newPosKey);
                    queue.push([newPos, steps + 1]);

                    if (status === 2) {
                        return steps + 1;
                    }

                    const oppositeDirection = this.getOppositeDirection(direction);
                    this.sendMoveCommand(oppositeDirection);
                }
            }
        }

        return -1;
    }

    getOppositeDirection(direction: number): number {
        const opposites: { [key: number]: number } = { 1: 2, 2: 1, 3: 4, 4: 3 };
        return opposites[direction];
    }

    moveTo(target: [number, number]): void {
        const path = this.findPath(this.currentPosition, target);
        for (const direction of path) {
            const status = this.sendMoveCommand(direction);
            if (status === 0) {
                throw new Error(`Unexpected wall while moving to ${target}`);
            }
            const [dx, dy] = this.directionMap[direction];
            this.currentPosition = [this.currentPosition[0] + dx, this.currentPosition[1] + dy];

            if (status === 2) {
                this.oxygenPosition = this.currentPosition;
            }
        }
    }

    findPath(start: [number, number], end: [number, number]): number[] {
        const queue: [[number, number], number[]][] = [[start, []]];
        const visited = new Set<string>([this.getKey(start)]);

        while (queue.length > 0) {
            const [position, path] = queue.shift()!;
            if (position[0] === end[0] && position[1] === end[1]) {
                return path;
            }

            for (let direction = 1; direction <= 4; direction++) {
                const [dx, dy] = this.directionMap[direction];
                const newPos: [number, number] = [position[0] + dx, position[1] + dy];
                const newPosKey = this.getKey(newPos);

                if (visited.has(newPosKey) || this.grid[newPosKey] === 0) {
                    continue;
                }

                visited.add(newPosKey);
                queue.push([newPos, [...path, direction]]);
            }
        }
        throw new Error(`No path found from ${start} to ${end}.`);
    }
}

function parseInput(filePath: string): number[] {
    const content = fs.readFileSync(filePath, 'utf-8').trim();
    return content.split(',').map(Number);
}

function main(): void {
    const inputFile = 'input.txt';
    const program = parseInput(inputFile);
    const droid = new Droid(program);
    const steps = droid.explore();

    if (steps !== -1) {
        console.log(`Fewest number of movement commands to reach the oxygen system: ${steps}`);
    } else {
        console.log("Oxygen system not found.");
    }
}

main();
