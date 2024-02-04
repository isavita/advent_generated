const fs = require('fs');

const program = fs.readFileSync('input.txt', 'utf8').trim().split(',');

const memory = {};
program.forEach((s, i) => {
    const value = parseInt(s, 10);
    memory[i] = value;
});

function runIntcode(memory, input) {
    let output = 0;
    let ip = 0;
    let relativeBase = 0;

    while (true) {
        const opcode = memory[ip] % 100;
        const modes = Math.floor(memory[ip] / 100).toString();

        const getParam = (offset) => {
            const mode = modes.length >= offset ? parseInt(modes[modes.length - offset], 10) : 0;
            const param = memory[ip + offset];
            switch (mode) {
                case 0:
                    return memory[param] || 0;
                case 1:
                    return param;
                case 2:
                    return memory[relativeBase + param] || 0;
                default:
                    throw new Error('unknown parameter mode');
            }
        };

        const setParam = (offset, value) => {
            const mode = modes.length >= offset ? parseInt(modes[modes.length - offset], 10) : 0;
            const param = memory[ip + offset];
            switch (mode) {
                case 0:
                    memory[param] = value;
                    break;
                case 2:
                    memory[relativeBase + param] = value;
                    break;
                default:
                    throw new Error('unknown parameter mode');
            }
        };

        switch (opcode) {
            case 1:
                setParam(3, getParam(1) + getParam(2));
                ip += 4;
                break;
            case 2:
                setParam(3, getParam(1) * getParam(2));
                ip += 4;
                break;
            case 3:
                setParam(1, input);
                ip += 2;
                break;
            case 4:
                output = getParam(1);
                ip += 2;
                break;
            case 5:
                ip = getParam(1) !== 0 ? getParam(2) : ip + 3;
                break;
            case 6:
                ip = getParam(1) === 0 ? getParam(2) : ip + 3;
                break;
            case 7:
                setParam(3, getParam(1) < getParam(2) ? 1 : 0);
                ip += 4;
                break;
            case 8:
                setParam(3, getParam(1) === getParam(2) ? 1 : 0);
                ip += 4;
                break;
            case 9:
                relativeBase += getParam(1);
                ip += 2;
                break;
            case 99:
                return output;
            default:
                throw new Error(`unknown opcode: ${opcode}`);
        }
    }
}

console.log(runIntcode(memory, 2));