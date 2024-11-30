const fs = require("fs");

// Read Intcode program from input file
const program = fs.readFileSync("input.txt", "utf8").trim().split(",").map(Number);

// Intcode computer implementation
function createIntcodeComputer(program, inputQueue) {
    let memory = [...program];
    let pointer = 0;
    let relativeBase = 0;
    let output = [];
    let halted = false;

    const getValue = (mode, param) => {
        if (mode === 0) return memory[param] || 0; // Position mode
        if (mode === 1) return param;             // Immediate mode
        if (mode === 2) return memory[relativeBase + param] || 0; // Relative mode
    };

    const setValue = (mode, param, value) => {
        if (mode === 0) memory[param] = value;   // Position mode
        if (mode === 2) memory[relativeBase + param] = value; // Relative mode
    };

    const run = () => {
        while (memory[pointer] !== 99) {
            const instruction = memory[pointer].toString().padStart(5, "0");
            const opCode = +instruction.slice(-2);
            const modes = instruction.slice(0, -2).split("").reverse().map(Number);

            const param1 = memory[pointer + 1];
            const param2 = memory[pointer + 2];
            const param3 = memory[pointer + 3];

            if (opCode === 1 || opCode === 2) {
                const val1 = getValue(modes[0], param1);
                const val2 = getValue(modes[1], param2);
                setValue(modes[2], param3, opCode === 1 ? val1 + val2 : val1 * val2);
                pointer += 4;
            } else if (opCode === 3) {
                if (inputQueue.length === 0) return;
                setValue(modes[0], param1, inputQueue.shift());
                pointer += 2;
            } else if (opCode === 4) {
                output.push(getValue(modes[0], param1));
                pointer += 2;
                if (output.length === 3) return output;
            } else if (opCode === 5 || opCode === 6) {
                const val1 = getValue(modes[0], param1);
                if ((opCode === 5 && val1 !== 0) || (opCode === 6 && val1 === 0)) {
                    pointer = getValue(modes[1], param2);
                } else {
                    pointer += 3;
                }
            } else if (opCode === 7 || opCode === 8) {
                const val1 = getValue(modes[0], param1);
                const val2 = getValue(modes[1], param2);
                setValue(modes[2], param3, opCode === 7 ? (val1 < val2 ? 1 : 0) : (val1 === val2 ? 1 : 0));
                pointer += 4;
            } else if (opCode === 9) {
                relativeBase += getValue(modes[0], param1);
                pointer += 2;
            }
        }
        halted = true;
    };

    return { run, output, halted };
}

// Initialize network
const NUM_COMPUTERS = 50;
const computers = [];
const queues = Array.from({ length: NUM_COMPUTERS }, () => []);
let natPacket = null;
let lastNatY = null;

for (let i = 0; i < NUM_COMPUTERS; i++) {
    const inputQueue = [i];
    const computer = createIntcodeComputer(program, inputQueue);
    computers.push({ computer, inputQueue });
}

// Run network
while (true) {
    let idle = true;

    for (let i = 0; i < NUM_COMPUTERS; i++) {
        const { computer, inputQueue } = computers[i];

        if (inputQueue.length === 0) inputQueue.push(-1);

        const result = computer.run();
        if (result) {
            idle = false;
            const [address, x, y] = result;
            computer.output.length = 0;

            if (address === 255) {
                natPacket = [x, y];
            } else {
                queues[address].push(x, y);
            }
        }
    }

    if (idle) {
        if (natPacket) {
            const [x, y] = natPacket;
            if (y === lastNatY) {
                console.log("First Y value sent by NAT to address 0 twice in a row:", y);
                break;
            }
            lastNatY = y;
            queues[0].push(x, y);
        }
    }

    for (let i = 0; i < NUM_COMPUTERS; i++) {
        if (queues[i].length > 0) {
            computers[i].inputQueue.push(...queues[i]);
            queues[i] = [];
        }
    }
}
