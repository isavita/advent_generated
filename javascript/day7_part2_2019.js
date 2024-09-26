const fs = require('fs');

// Intcode Computer Class
class IntcodeComputer {
    constructor(program, inputs = []) {
        this.memory = [...program];
        this.pointer = 0;
        this.inputs = inputs;
        this.outputs = [];
        this.halted = false;
    }

    getParam(mode, value) {
        return mode === 1 ? value : this.memory[value];
    }

    run() {
        while (this.pointer < this.memory.length) {
            let instruction = this.memory[this.pointer];
            let opcode = instruction % 100;
            let modes = Math.floor(instruction / 100);
            let mode1 = modes % 10;
            let mode2 = Math.floor(modes / 10) % 10;
            let mode3 = Math.floor(modes / 100) % 10;

            switch (opcode) {
                case 1: { // Add
                    const param1 = this.getParam(mode1, this.memory[this.pointer + 1]);
                    const param2 = this.getParam(mode2, this.memory[this.pointer + 2]);
                    const dest = this.memory[this.pointer + 3];
                    this.memory[dest] = param1 + param2;
                    this.pointer += 4;
                    break;
                }
                case 2: { // Multiply
                    const param1 = this.getParam(mode1, this.memory[this.pointer + 1]);
                    const param2 = this.getParam(mode2, this.memory[this.pointer + 2]);
                    const dest = this.memory[this.pointer + 3];
                    this.memory[dest] = param1 * param2;
                    this.pointer += 4;
                    break;
                }
                case 3: { // Input
                    if (this.inputs.length === 0) {
                        return; // Wait for input
                    }
                    const dest = this.memory[this.pointer + 1];
                    this.memory[dest] = this.inputs.shift();
                    this.pointer += 2;
                    break;
                }
                case 4: { // Output
                    const param1 = this.getParam(mode1, this.memory[this.pointer + 1]);
                    this.outputs.push(param1);
                    this.pointer += 2;
                    return param1; // Yield output
                }
                case 5: { // Jump-if-true
                    const param1 = this.getParam(mode1, this.memory[this.pointer + 1]);
                    const param2 = this.getParam(mode2, this.memory[this.pointer + 2]);
                    if (param1 !== 0) {
                        this.pointer = param2;
                    } else {
                        this.pointer += 3;
                    }
                    break;
                }
                case 6: { // Jump-if-false
                    const param1 = this.getParam(mode1, this.memory[this.pointer + 1]);
                    const param2 = this.getParam(mode2, this.memory[this.pointer + 2]);
                    if (param1 === 0) {
                        this.pointer = param2;
                    } else {
                        this.pointer += 3;
                    }
                    break;
                }
                case 7: { // Less than
                    const param1 = this.getParam(mode1, this.memory[this.pointer + 1]);
                    const param2 = this.getParam(mode2, this.memory[this.pointer + 2]);
                    const dest = this.memory[this.pointer + 3];
                    this.memory[dest] = param1 < param2 ? 1 : 0;
                    this.pointer += 4;
                    break;
                }
                case 8: { // Equals
                    const param1 = this.getParam(mode1, this.memory[this.pointer + 1]);
                    const param2 = this.getParam(mode2, this.memory[this.pointer + 2]);
                    const dest = this.memory[this.pointer + 3];
                    this.memory[dest] = param1 === param2 ? 1 : 0;
                    this.pointer += 4;
                    break;
                }
                case 99: { // Halt
                    this.halted = true;
                    return;
                }
                default:
                    throw new Error(`Unknown opcode ${opcode} at position ${this.pointer}`);
            }
        }
    }
}

// Function to generate all permutations of an array
function* permutations(array, n = array.length) {
    if (n === 1) {
        yield array.slice();
    } else {
        for (let i = 0; i < n; i++) {
            yield* permutations(array, n - 1);
            const j = n % 2 ? 0 : i;
            [array[j], array[n - 1]] = [array[n - 1], array[j]];
        }
    }
}

// Read Intcode program from input.txt
const program = fs.readFileSync('input.txt', 'utf-8').trim().split(',').map(Number);

// Part One: Series Configuration
function partOne(program) {
    const phaseSettings = [0, 1, 2, 3, 4];
    let maxSignal = 0;

    for (let phaseSeq of permutations(phaseSettings)) {
        let input = 0;
        for (let phase of phaseSeq) {
            const computer = new IntcodeComputer(program, [phase, input]);
            computer.run();
            input = computer.outputs[0];
        }
        if (input > maxSignal) {
            maxSignal = input;
        }
    }

    return maxSignal;
}

// Part Two: Feedback Loop Configuration
function partTwo(program) {
    const phaseSettings = [5, 6, 7, 8, 9];
    let maxSignal = 0;

    for (let phaseSeq of permutations(phaseSettings)) {
        // Initialize amplifiers
        const amplifiers = phaseSeq.map(phase => new IntcodeComputer(program, [phase]));
        let input = 0;
        let lastOutput = 0;
        let halted = false;
        let currentAmp = 0;

        while (!halted) {
            const amp = amplifiers[currentAmp];
            amp.inputs.push(input);
            const output = amp.run();
            if (output !== undefined) {
                input = output;
                if (currentAmp === amplifiers.length - 1) {
                    lastOutput = input;
                }
            } else {
                // Check if all amplifiers have halted
                halted = amplifiers.every(a => a.halted);
            }
            currentAmp = (currentAmp + 1) % amplifiers.length;
        }

        if (lastOutput > maxSignal) {
            maxSignal = lastOutput;
        }
    }

    return maxSignal;
}

// Execute and print results
const resultPartOne = partOne(program);
console.log(`Part One: The highest signal that can be sent to the thrusters is ${resultPartOne}.`);

const resultPartTwo = partTwo(program);
console.log(`Part Two: The highest signal that can be sent to the thrusters with feedback loop is ${resultPartTwo}.`);
