const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(',');

const memory = new Map();
input.forEach((value, index) => {
    memory.set(index, parseInt(value));
});

function runIntcode(memory) {
    let output = 0;
    let ip = 0;
    let relativeBase = 0;

    while (true) {
        const opcode = memory.get(ip) % 100;
        const modes = Math.floor(memory.get(ip) / 100).toString();

        const getParam = (offset) => {
            const mode = modes.length >= offset ? parseInt(modes[modes.length - offset]) : 0;
            const param = memory.get(ip + offset);
            switch (mode) {
                case 0:
                    return memory.get(param) || 0;
                case 1:
                    return param;
                case 2:
                    return memory.get(relativeBase + param) || 0;
                default:
                    throw new Error('unknown parameter mode');
            }
        };

        const setParam = (offset, value) => {
            const mode = modes.length >= offset ? parseInt(modes[modes.length - offset]) : 0;
            const param = memory.get(ip + offset);
            switch (mode) {
                case 0:
                    memory.set(param, value);
                    break;
                case 2:
                    memory.set(relativeBase + param, value);
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
                setParam(1, 1); // Test mode input
                ip += 2;
                break;
            case 4:
                output = getParam(1);
                ip += 2;
                break;
            case 5:
                if (getParam(1) !== 0) {
                    ip = getParam(2);
                } else {
                    ip += 3;
                }
                break;
            case 6:
                if (getParam(1) === 0) {
                    ip = getParam(2);
                } else {
                    ip += 3;
                }
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

console.log(runIntcode(memory));