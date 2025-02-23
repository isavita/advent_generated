
import * as fs from 'fs';

class IntcodeComputer {
    memory: Map<number, number>;
    pointer: number;
    relativeBase: number;
    inputs: number[];
    outputs: number[];
    halted: boolean;

    constructor(memory: number[]) {
        this.memory = new Map<number, number>();
        for (let i = 0; i < memory.length; i++) {
            this.memory.set(i, memory[i]);
        }
        this.pointer = 0;
        this.relativeBase = 0;
        this.inputs = [];
        this.outputs = [];
        this.halted = false;
    }

    getParam(mode: number, param: number): number {
        switch (mode) {
            case 0:
                return this.memory.get(param) || 0;
            case 1:
                return param;
            case 2:
                return this.memory.get(this.relativeBase + param) || 0;
            default:
                throw new Error(`Unknown parameter mode: ${mode}`);
        }
    }

    setParam(mode: number, param: number, value: number): void {
        switch (mode) {
            case 0:
                this.memory.set(param, value);
                break;
            case 2:
                this.memory.set(this.relativeBase + param, value);
                break;
            default:
                throw new Error(`Unknown parameter mode for writing: ${mode}`);
        }
    }

    run(): void {
        while (true) {
            const instruction = this.memory.get(this.pointer) || 0;
            const opcode = instruction % 100;
            const modes = [
                Math.floor((instruction / 100) % 10),
                Math.floor((instruction / 1000) % 10),
                Math.floor((instruction / 10000) % 10)
            ];

            if (opcode === 1) {
                const param1 = this.memory.get(this.pointer + 1) || 0;
                const param2 = this.memory.get(this.pointer + 2) || 0;
                const param3 = this.memory.get(this.pointer + 3) || 0;
                const val1 = this.getParam(modes[0], param1);
                const val2 = this.getParam(modes[1], param2);
                this.setParam(modes[2], param3, val1 + val2);
                this.pointer += 4;
            } else if (opcode === 2) {
                const param1 = this.memory.get(this.pointer + 1) || 0;
                const param2 = this.memory.get(this.pointer + 2) || 0;
                const param3 = this.memory.get(this.pointer + 3) || 0;
                const val1 = this.getParam(modes[0], param1);
                const val2 = this.getParam(modes[1], param2);
                this.setParam(modes[2], param3, val1 * val2);
                this.pointer += 4;
            } else if (opcode === 3) {
                if (this.inputs.length === 0) {
                    return;
                }
                const param1 = this.memory.get(this.pointer + 1) || 0;
                const inputValue = this.inputs.shift()!;
                this.setParam(modes[0], param1, inputValue);
                this.pointer += 2;
            } else if (opcode === 4) {
                const param1 = this.memory.get(this.pointer + 1) || 0;
                const outputValue = this.getParam(modes[0], param1);
                this.outputs.push(outputValue);
                this.pointer += 2;
            } else if (opcode === 5) {
                const param1 = this.memory.get(this.pointer + 1) || 0;
                const param2 = this.memory.get(this.pointer + 2) || 0;
                const val1 = this.getParam(modes[0], param1);
                const val2 = this.getParam(modes[1], param2);
                if (val1 !== 0) {
                    this.pointer = val2;
                } else {
                    this.pointer += 3;
                }
            } else if (opcode === 6) {
                const param1 = this.memory.get(this.pointer + 1) || 0;
                const param2 = this.memory.get(this.pointer + 2) || 0;
                const val1 = this.getParam(modes[0], param1);
                const val2 = this.getParam(modes[1], param2);
                if (val1 === 0) {
                    this.pointer = val2;
                } else {
                    this.pointer += 3;
                }
            } else if (opcode === 7) {
                const param1 = this.memory.get(this.pointer + 1) || 0;
                const param2 = this.memory.get(this.pointer + 2) || 0;
                const param3 = this.memory.get(this.pointer + 3) || 0;
                const val1 = this.getParam(modes[0], param1);
                const val2 = this.getParam(modes[1], param2);
                this.setParam(modes[2], param3, val1 < val2 ? 1 : 0);
                this.pointer += 4;
            } else if (opcode === 8) {
                const param1 = this.memory.get(this.pointer + 1) || 0;
                const param2 = this.memory.get(this.pointer + 2) || 0;
                const param3 = this.memory.get(this.pointer + 3) || 0;
                const val1 = this.getParam(modes[0], param1);
                const val2 = this.getParam(modes[1], param2);
                this.setParam(modes[2], param3, val1 === val2 ? 1 : 0);
                this.pointer += 4;
            } else if (opcode === 9) {
                const param1 = this.memory.get(this.pointer + 1) || 0;
                const val1 = this.getParam(modes[0], param1);
                this.relativeBase += val1;
                this.pointer += 2;
            } else if (opcode === 99) {
                this.halted = true;
                return;
            } else {
                throw new Error(`Unknown opcode: ${opcode}`);
            }
        }
    }
}

function readInput(filename: string): number[] {
    const content = fs.readFileSync(filename, 'utf-8').trim();
    return content.split(',').map(x => parseInt(x));
}

function parseMap(output: number[]): string[][] {
    const grid: string[][] = [];
    let line: string[] = [];
    for (const c of output) {
        if (c === 10) {
            if (line.length > 0) {
                grid.push(line);
                line = [];
            }
        } else {
            line.push(String.fromCharCode(c));
        }
    }
    if (line.length > 0) {
        grid.push(line);
    }
    return grid;
}

function findIntersections(grid: string[][]): [number, number][] {
    const intersections: [number, number][] = [];
    for (let y = 1; y < grid.length - 1; y++) {
        for (let x = 1; x < grid[0].length - 1; x++) {
            if (grid[y][x] !== '#') {
                continue;
            }
            if (
                grid[y - 1][x] === '#' &&
                grid[y + 1][x] === '#' &&
                grid[y][x - 1] === '#' &&
                grid[y][x + 1] === '#'
            ) {
                intersections.push([x, y]);
            }
        }
    }
    return intersections;
}

function findRobotPosition(grid: string[][]): [number, number, string] | null {
    for (let y = 0; y < grid.length; y++) {
        for (let x = 0; x < grid[0].length; x++) {
            const cell = grid[y][x];
            if (['^', 'v', '<', '>', 'X'].includes(cell)) {
                return [x, y, cell];
            }
        }
    }
    return null;
}

function turnLeft(direction: string): string {
    const turns: { [key: string]: string } = { '^': '<', '<': 'v', 'v': '>', '>': '^' };
    return turns[direction];
}

function turnRight(direction: string): string {
    const turns: { [key: string]: string } = { '^': '>', '>': 'v', 'v': '<', '<': '^' };
    return turns[direction];
}

function moveForward(x: number, y: number, direction: string): [number, number] {
    switch (direction) {
        case '^':
            return [x, y - 1];
        case 'v':
            return [x, y + 1];
        case '<':
            return [x - 1, y];
        case '>':
            return [x + 1, y];
        default:
            throw new Error(`Unknown direction: ${direction}`);
    }
}

function getMovementPath(grid: string[][], startX: number, startY: number, startDir: string): string[] {
    let x = startX;
    let y = startY;
    let direction = startDir;
    const path: string[] = [];
    let steps = 0;

    while (true) {
        const [nextX, nextY] = moveForward(x, y, direction);

        if (nextY >= 0 && nextY < grid.length && nextX >= 0 && nextX < grid[0].length && grid[nextY][nextX] === '#') {
            x = nextX;
            y = nextY;
            steps++;
        } else {
            if (steps > 0) {
                path.push(steps.toString());
                steps = 0;
            }

            const leftDir = turnLeft(direction);
            const [nextLeftX, nextLeftY] = moveForward(x, y, leftDir);
            if (nextLeftY >= 0 && nextLeftY < grid.length && nextLeftX >= 0 && nextLeftX < grid[0].length && grid[nextLeftY][nextLeftX] === '#') {
                path.push('L');
                direction = leftDir;
                continue;
            }

            const rightDir = turnRight(direction);
            const [nextRightX, nextRightY] = moveForward(x, y, rightDir);
            if (nextRightY >= 0 && nextRightY < grid.length && nextRightX >= 0 && nextRightX < grid[0].length && grid[nextRightY][nextRightX] === '#') {
                path.push('R');
                direction = rightDir;
                continue;
            }

            break;
        }
    }

    return path;
}

function compressMovement(path: string[]): [string, string, string, string] {
    const maxFunctionLength = 20;
    const maxPatternLength = 10;
    const tokens = path.join(',').split(',');

    function replaceSequence(seq: string[], pattern: string[], replacement: string): string[] {
        const res: string[] = [];
        let i = 0;
        while (i < seq.length) {
            if (seq.slice(i, i + pattern.length).join(',') === pattern.join(',')) {
                res.push(replacement);
                i += pattern.length;
            } else {
                res.push(seq[i]);
                i++;
            }
        }
        return res;
    }

    for (let aLen = 1; aLen <= maxPatternLength; aLen++) {
        const aPattern = tokens.slice(0, aLen);
        const aStr = aPattern.join(',');
        if (aStr.length > maxFunctionLength) continue;
        const tokensAfterA = replaceSequence(tokens, aPattern, 'A');

        for (let bStart = aLen; bStart < tokens.length; bStart++) {
            for (let bLen = 1; bLen <= maxPatternLength; bLen++) {
                const bPattern = tokens.slice(bStart, bStart + bLen);
                const bStr = bPattern.join(',');
                if (bStr.length > maxFunctionLength) continue;
                const tokensAfterB = replaceSequence(tokensAfterA, bPattern, 'B');

                for (let cStart = bStart + bLen; cStart < tokens.length; cStart++) {
                    for (let cLen = 1; cLen <= maxPatternLength; cLen++) {
                        const cPattern = tokens.slice(cStart, cStart + cLen);
                        const cStr = cPattern.join(',');
                        if (cStr.length > maxFunctionLength) continue;
                        const tokensAfterC = replaceSequence(tokensAfterB, cPattern, 'C');

                        let mainTokens = tokensAfterC.slice();
                        let changed = true;
                        while (changed) {
                            changed = false;
                            const tempTokens = mainTokens.slice();
                            mainTokens = [];
                            let i = 0;
                            while (i < tempTokens.length) {
                                if (tempTokens.slice(i, i + aPattern.length).join(',') === aPattern.join(',')) {
                                    mainTokens.push('A');
                                    i += aPattern.length;
                                    changed = true;
                                } else if (tempTokens.slice(i, i + bPattern.length).join(',') === bPattern.join(',')) {
                                    mainTokens.push('B');
                                    i += bPattern.length;
                                    changed = true;
                                } else if (tempTokens.slice(i, i + cPattern.length).join(',') === cPattern.join(',')) {
                                    mainTokens.push('C');
                                    i += cPattern.length;
                                    changed = true;
                                } else {
                                    mainTokens.push(tempTokens[i]);
                                    i++;
                                }
                            }
                        }

                        const mainRoutine = mainTokens.join(',');
                        if (mainRoutine.split('').every(c => 'ABC,'.includes(c)) && mainRoutine.length <= maxFunctionLength) {
                            const functionA = aPattern.join(',');
                            const functionB = bPattern.join(',');
                            const functionC = cPattern.join(',');
                            if (functionA.length <= maxFunctionLength && functionB.length <= maxFunctionLength && functionC.length <= maxFunctionLength) {
                                return [mainRoutine, functionA, functionB, functionC];
                            }
                        }
                    }
                }
            }
        }
    }

    throw new Error("Could not compress the path into functions A, B, C.");
}

function main(): void {
    const program = readInput('input.txt');

    // Part One
    let computer = new IntcodeComputer([...program]);
    computer.run();
    const output = computer.outputs;
    const grid = parseMap(output);
    const intersections = findIntersections(grid);
    const alignmentSum = intersections.reduce((sum, [x, y]) => sum + x * y, 0);
    console.log(`Part One: Sum of alignment parameters = ${alignmentSum}`);

    // Part Two
    const programPart2 = [...program];
    programPart2[0] = 2;
    const computerPart2 = new IntcodeComputer(programPart2);

    const robot = findRobotPosition(grid);
    if (!robot) {
        throw new Error("Robot not found on the scaffold.");
    }
    const [startX, startY, startDir] = robot;

    const movementPath = getMovementPath(grid, startX, startY, startDir);

    let mainRoutine: string, functionA: string, functionB: string, functionC: string;
    try {
        [mainRoutine, functionA, functionB, functionC] = compressMovement(movementPath);
    } catch (e: any) {
        console.error("Error in compressing path:", e);
        process.exit(1);
    }

    const inputLines = [
        mainRoutine,
        functionA,
        functionB,
        functionC,
        'n'
    ];
    const movementInputs = inputLines.flatMap(line => line.split('').map(char => char.charCodeAt(0)).concat([10]));

    computerPart2.inputs = movementInputs;

    while (!computerPart2.halted) {
        computerPart2.run();
    }

    let dustCollected = 0;
    while (computerPart2.outputs.length > 0) {
        dustCollected = computerPart2.outputs.shift()!;
    }

    console.log(`Part Two: Dust collected = ${dustCollected}`);
}

main();
