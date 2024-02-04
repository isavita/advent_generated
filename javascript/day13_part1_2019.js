
const fs = require('fs');

const program = fs.readFileSync('input.txt', 'utf8').trim().split(',').map(Number);

const countBlocks = (program) => {
    const grid = new Map();
    const inStream = [];
    const outStream = runProgram(program, inStream);
    
    while (true) {
        const x = outStream.next().value;
        if (x === undefined) break;
        const y = outStream.next().value;
        const tile = outStream.next().value;
        grid.set(`${x},${y}`, tile);
    }
    
    let blockCount = 0;
    for (const t of grid.values()) {
        if (t === 2) {
            blockCount++;
        }
    }
    
    return blockCount;
};

const runProgram = function*(program, inStream) {
    const data = new Map();
    let ip = 0;
    let relbase = 0;
    
    for (let i = 0; i < program.length; i++) {
        data.set(i, program[i]);
    }
    
    const get = (i, mo) => {
        switch (mo) {
            case 0:
                return data.get(data.get(i)) || 0;
            case 1:
                return data.get(i) || 0;
            case 2:
                return data.get(relbase + data.get(i)) || 0;
            default:
                throw new Error(`Unknown mode: ${mo}`);
        }
    };
    
    const set = (i, mo, val) => {
        switch (mo) {
            case 0:
                data.set(data.get(i), val);
                break;
            case 2:
                data.set(relbase + data.get(i), val);
                break;
            default:
                throw new Error(`Unknown mode: ${mo}`);
        }
    };
    
    while (true) {
        const op = data.get(ip) % 100;
        const modes = [(data.get(ip) / 100) % 10 | 0, (data.get(ip) / 1000) % 10 | 0, (data.get(ip) / 10000) % 10 | 0];
        
        switch (op) {
            case 1:
                set(ip + 3, modes[2], get(ip + 1, modes[0]) + get(ip + 2, modes[1]));
                ip += 4;
                break;
            case 2:
                set(ip + 3, modes[2], get(ip + 1, modes[0]) * get(ip + 2, modes[1]));
                ip += 4;
                break;
            case 3:
                set(ip + 1, modes[0], inStream.shift());
                ip += 2;
                break;
            case 4:
                yield get(ip + 1, modes[0]);
                ip += 2;
                break;
            case 5:
                if (get(ip + 1, modes[0]) !== 0) {
                    ip = get(ip + 2, modes[1]);
                } else {
                    ip += 3;
                }
                break;
            case 6:
                if (get(ip + 1, modes[0]) === 0) {
                    ip = get(ip + 2, modes[1]);
                } else {
                    ip += 3;
                }
                break;
            case 7:
                set(ip + 3, modes[2], get(ip + 1, modes[0]) < get(ip + 2, modes[1]) ? 1 : 0);
                ip += 4;
                break;
            case 8:
                set(ip + 3, modes[2], get(ip + 1, modes[0]) === get(ip + 2, modes[1]) ? 1 : 0);
                ip += 4;
                break;
            case 9:
                relbase += get(ip + 1, modes[0]);
                ip += 2;
                break;
            case 99:
                return;
            default:
                throw new Error(`Unknown opcode: ${op}`);
        }
    }
};

console.log(countBlocks(program));
