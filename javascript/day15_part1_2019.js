const fs = require('fs');
const path = require('path');

// Direction mappings
const DIRECTIONS = {
    1: { name: 'north', dx: 0, dy: -1 },
    2: { name: 'south', dx: 0, dy: 1 },
    3: { name: 'west',  dx: -1, dy: 0 },
    4: { name: 'east',  dx: 1, dy: 0 },
};

const OPPOSITE_DIRECTION = {
    1: 2,
    2: 1,
    3: 4,
    4: 3,
};

// Status codes
const STATUS = {
    WALL: 0,
    MOVED: 1,
    OXYGEN: 2,
};

// Intcode Computer Class
class IntcodeComputer {
    constructor(program) {
        this.memory = program.slice();
        this.pointer = 0;
        this.relativeBase = 0;
        this.inputs = [];
        this.outputs = [];
        this.halted = false;
    }

    getParam(mode, param) {
        if (mode === 0) { // Position mode
            return this.memory[param] || 0;
        } else if (mode === 1) { // Immediate mode
            return param;
        } else if (mode === 2) { // Relative mode
            return this.memory[this.relativeBase + param] || 0;
        }
    }

    setParam(mode, param, value) {
        if (mode === 0) { // Position mode
            this.memory[param] = value;
        } else if (mode === 2) { // Relative mode
            this.memory[this.relativeBase + param] = value;
        } else {
            throw new Error(`Unsupported parameter mode for writing: ${mode}`);
        }
    }

    addInput(value) {
        this.inputs.push(value);
    }

    getOutput() {
        return this.outputs.shift();
    }

    run() {
        while (this.pointer < this.memory.length) {
            const instruction = this.memory[this.pointer];
            const opcode = instruction % 100;
            const mode1 = Math.floor(instruction / 100) % 10;
            const mode2 = Math.floor(instruction / 1000) % 10;
            const mode3 = Math.floor(instruction / 10000) % 10;

            if (opcode === 99) {
                this.halted = true;
                break;
            }

            let param1 = this.memory[this.pointer + 1];
            let param2 = this.memory[this.pointer + 2];
            let param3 = this.memory[this.pointer + 3];

            switch (opcode) {
                case 1: { // Addition
                    const val1 = this.getParam(mode1, param1);
                    const val2 = this.getParam(mode2, param2);
                    this.setParam(mode3, param3, val1 + val2);
                    this.pointer += 4;
                    break;
                }
                case 2: { // Multiplication
                    const val1 = this.getParam(mode1, param1);
                    const val2 = this.getParam(mode2, param2);
                    this.setParam(mode3, param3, val1 * val2);
                    this.pointer += 4;
                    break;
                }
                case 3: { // Input
                    if (this.inputs.length === 0) {
                        return; // Wait for input
                    }
                    const input = this.inputs.shift();
                    this.setParam(mode1, param1, input);
                    this.pointer += 2;
                    break;
                }
                case 4: { // Output
                    const output = this.getParam(mode1, param1);
                    this.outputs.push(output);
                    this.pointer += 2;
                    return; // Pause after output
                }
                case 5: { // Jump-if-true
                    const val1 = this.getParam(mode1, param1);
                    const val2 = this.getParam(mode2, param2);
                    if (val1 !== 0) {
                        this.pointer = val2;
                    } else {
                        this.pointer += 3;
                    }
                    break;
                }
                case 6: { // Jump-if-false
                    const val1 = this.getParam(mode1, param1);
                    const val2 = this.getParam(mode2, param2);
                    if (val1 === 0) {
                        this.pointer = val2;
                    } else {
                        this.pointer += 3;
                    }
                    break;
                }
                case 7: { // Less than
                    const val1 = this.getParam(mode1, param1);
                    const val2 = this.getParam(mode2, param2);
                    this.setParam(mode3, param3, val1 < val2 ? 1 : 0);
                    this.pointer += 4;
                    break;
                }
                case 8: { // Equals
                    const val1 = this.getParam(mode1, param1);
                    const val2 = this.getParam(mode2, param2);
                    this.setParam(mode3, param3, val1 === val2 ? 1 : 0);
                    this.pointer += 4;
                    break;
                }
                case 9: { // Adjust relative base
                    const val1 = this.getParam(mode1, param1);
                    this.relativeBase += val1;
                    this.pointer += 2;
                    break;
                }
                default:
                    throw new Error(`Unknown opcode: ${opcode} at position ${this.pointer}`);
            }
        }
    }
}

// Position utility
class Position {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }

    toString() {
        return `${this.x},${this.y}`;
    }

    equals(other) {
        return this.x === other.x && this.y === other.y;
    }
}

// Map Explorer Class using DFS
class MapExplorer {
    constructor(computer) {
        this.computer = computer;
        this.map = new Map(); // Key: "x,y", Value: status
        this.start = new Position(0, 0);
        this.currentPos = this.start;
        this.map.set(this.start.toString(), STATUS.MOVED);
        this.oxygenPos = null;
        this.visited = new Set();
        this.visited.add(this.start.toString());
    }

    async explore() {
        await this.dfs(this.currentPos);
    }

    async dfs(pos) {
        for (const [cmd, dir] of Object.entries(DIRECTIONS)) {
            const newX = pos.x + dir.dx;
            const newY = pos.y + dir.dy;
            const newPos = new Position(newX, newY);
            const posKey = newPos.toString();

            if (this.visited.has(posKey)) {
                continue;
            }

            // Send movement command
            this.computer.addInput(parseInt(cmd));
            this.computer.run();

            const status = this.computer.getOutput();

            if (status === STATUS.WALL) {
                // Wall detected; mark on map
                this.map.set(posKey, STATUS.WALL);
            } else {
                // Moved successfully
                this.map.set(posKey, status);
                this.visited.add(posKey);

                if (status === STATUS.OXYGEN) {
                    this.oxygenPos = newPos;
                }

                // Recursive exploration
                await this.dfs(newPos);

                // Backtrack: move in the opposite direction
                const backCmd = OPPOSITE_DIRECTION[cmd];
                this.computer.addInput(backCmd);
                this.computer.run();
                const backStatus = this.computer.getOutput();

                if (backStatus === STATUS.WALL) {
                    throw new Error("Failed to backtrack!");
                }
            }
        }
    }
}

// BFS to find the shortest path on the mapped maze
function bfs(map, startKey, endKey) {
    const queue = [{ key: startKey, steps: 0 }];
    const visited = new Set();
    visited.add(startKey);

    while (queue.length > 0) {
        const current = queue.shift();
        const { key, steps } = current;

        if (key === endKey) {
            return steps;
        }

        const [x, y] = key.split(',').map(Number);

        for (const [cmd, dir] of Object.entries(DIRECTIONS)) {
            const newX = x + dir.dx;
            const newY = y + dir.dy;
            const newKey = `${newX},${newY}`;

            if (map.get(newKey) && map.get(newKey) !== STATUS.WALL && !visited.has(newKey)) {
                visited.add(newKey);
                queue.push({ key: newKey, steps: steps + 1 });
            }
        }
    }

    return -1; // Path not found
}

// Main Function
async function main() {
    // Read input.txt
    const inputPath = path.join(__dirname, 'input.txt');
    const input = fs.readFileSync(inputPath, 'utf8').trim();
    const program = input.split(',').map(Number);

    // Initialize Intcode computer
    const computer = new IntcodeComputer(program);

    // Initialize Map Explorer
    const explorer = new MapExplorer(computer);

    console.log("Exploring the maze. Please wait...");

    // Explore the map
    await explorer.explore();

    if (!explorer.oxygenPos) {
        console.log("Oxygen system not found.");
        return;
    }

    console.log("Map exploration completed.");

    // Perform BFS on the mapped maze to find the shortest path
    const startKey = explorer.start.toString();
    const endKey = explorer.oxygenPos.toString();
    const steps = bfs(explorer.map, startKey, endKey);

    if (steps !== -1) {
        console.log(`Fewest movement commands required: ${steps}`);
    } else {
        console.log("No path to oxygen system found.");
    }
}

main();
