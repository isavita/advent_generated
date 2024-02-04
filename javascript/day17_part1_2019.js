const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(',').map(Number);

function decode(n) {
    const op = n % 100;
    n = Math.floor(n / 100);
    const modes = [];
    for (let i = 0; i < 3; i++) {
        modes.push(n % 10);
        n = Math.floor(n / 10);
    }
    return [op, modes];
}

class Machine {
    constructor(program, input, output) {
        this.data = new Map();
        this.ip = 0;
        this.relbase = 0;
        this.in = input;
        this.out = output;
        for (let i = 0; i < program.length; i++) {
            this.data.set(i, program[i]);
        }
    }

    get(i, mo) {
        switch (mo) {
            case 0:
                return this.data.get(this.data.get(i)) || 0;
            case 1:
                return this.data.get(i) || 0;
            case 2:
                return this.data.get(this.relbase + this.data.get(i)) || 0;
            default:
                throw new Error(`Unknown mode: ${mo}`);
        }
    }

    set(i, mo, val) {
        switch (mo) {
            case 0:
                this.data.set(this.data.get(i), val);
                break;
            case 2:
                this.data.set(this.relbase + this.data.get(i), val);
                break;
            default:
                throw new Error(`Unknown mode: ${mo}`);
        }
    }

    step() {
        const [op, modes] = decode(this.data.get(this.ip));
        switch (op) {
            case 1:
                this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) + this.get(this.ip + 2, modes[1]));
                this.ip += 4;
                break;
            case 2:
                this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) * this.get(this.ip + 2, modes[1]));
                this.ip += 4;
                break;
            case 3:
                this.set(this.ip + 1, modes[0], this.in.shift());
                this.ip += 2;
                break;
            case 4:
                this.out.push(this.get(this.ip + 1, modes[0]));
                this.ip += 2;
                break;
            case 5:
                if (this.get(this.ip + 1, modes[0]) !== 0) {
                    this.ip = this.get(this.ip + 2, modes[1]);
                } else {
                    this.ip += 3;
                }
                break;
            case 6:
                if (this.get(this.ip + 1, modes[0]) === 0) {
                    this.ip = this.get(this.ip + 2, modes[1]);
                } else {
                    this.ip += 3;
                }
                break;
            case 7:
                this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) < this.get(this.ip + 2, modes[1]) ? 1 : 0);
                this.ip += 4;
                break;
            case 8:
                this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) === this.get(this.ip + 2, modes[1]) ? 1 : 0);
                this.ip += 4;
                break;
            case 9:
                this.relbase += this.get(this.ip + 1, modes[0]);
                this.ip += 2;
                break;
            case 99:
                return false;
            default:
                throw new Error(`Unknown opcode: ${op}`);
        }
        return true;
    }

    run() {
        while (this.step());
    }
}

function run(program, input) {
    const output = [];
    const machine = new Machine(program, input, output);
    machine.run();
    return output;
}

const [scaffolding, robot, dir] = parse(input);

console.log(sumAlign(scaffolding));

function parse(program) {
    const inBuffer = [];
    const outBuffer = run(program, inBuffer);
    let sb = '';
    outBuffer.forEach(o => sb += String.fromCharCode(o));

    const scaffolding = new Map();
    let robot;
    let robotDir;
    const lines = sb.trim().split('\n');
    lines.forEach((line, y) => {
        line.split('').forEach((c, x) => {
            switch (c) {
                case '^':
                case 'v':
                case '<':
                case '>':
                    robot = { x, y };
                    robotDir = c;
                    break;
                case '#':
                    scaffolding.set(`${x},${y}`, true);
                    break;
            }
        });
    });

    return [scaffolding, robot, robotDir];
}

function sumAlign(grid) {
    let sum = 0;
    for (const [p, _] of grid) {
        let valid = true;
        const [x, y] = p.split(',').map(Number);
        const neighbors = [[0, 1], [0, -1], [1, 0], [-1, 0]];
        for (const [dx, dy] of neighbors) {
            if (!grid.get(`${x + dx},${y + dy}`)) {
                valid = false;
                break;
            }
        }
        if (valid) {
            sum += x * y;
        }
    }
    return sum;
}