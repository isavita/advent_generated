
const fs = require('fs');

const position = 0;
const immediate = 1;
const relative = 2;

const add = 1;
const mul = 2;
const input = 3;
const output = 4;
const jt = 5;
const jf = 6;
const lt = 7;
const eq = 8;
const rbo = 9;
const halt = 99;

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
    constructor(program, input) {
        this.data = new Map();
        for (let i = 0; i < program.length; i++) {
            this.data.set(i, program[i]);
        }
        this.ip = 0;
        this.input = input;
        this.output = [];
        this.relbase = 0;
    }

    get(i, mode) {
        let val;
        if (mode === immediate) {
            val = this.data.get(i) || 0;
        } else if (mode === position) {
            val = this.data.get(this.data.get(i) || 0) || 0;
        } else if (mode === relative) {
            val = this.data.get(this.relbase + (this.data.get(i) || 0)) || 0;
        } else {
            throw new Error(`Unknown mode: ${mode}`);
        }
        return val;
    }

    set(i, mode, val) {
        if (mode === position) {
            this.data.set(this.data.get(i) || 0, val);
        } else if (mode === relative) {
            this.data.set(this.relbase + (this.data.get(i) || 0), val);
        } else {
            throw new Error(`Unknown mode: ${mode}`);
        }
    }

    step() {
        const [op, modes] = decode(this.data.get(this.ip) || 0);
        switch (op) {
            case add: {
                const val = this.get(this.ip + 1, modes[0]) + this.get(this.ip + 2, modes[1]);
                this.set(this.ip + 3, modes[2], val);
                this.ip += 4;
                break;
            }
            case mul: {
                const val = this.get(this.ip + 1, modes[0]) * this.get(this.ip + 2, modes[1]);
                this.set(this.ip + 3, modes[2], val);
                this.ip += 4;
                break;
            }
            case input: {
                this.set(this.ip + 1, modes[0], this.input.shift());
                this.ip += 2;
                break;
            }
            case output: {
                this.output.push(this.get(this.ip + 1, modes[0]));
                this.ip += 2;
                break;
            }
            case jt: {
                if (this.get(this.ip + 1, modes[0]) !== 0) {
                    this.ip = this.get(this.ip + 2, modes[1]);
                } else {
                    this.ip += 3;
                }
                break;
            }
            case jf: {
                if (this.get(this.ip + 1, modes[0]) === 0) {
                    this.ip = this.get(this.ip + 2, modes[1]);
                } else {
                    this.ip += 3;
                }
                break;
            }
            case lt: {
                const val = this.get(this.ip + 1, modes[0]) < this.get(this.ip + 2, modes[1]) ? 1 : 0;
                this.set(this.ip + 3, modes[2], val);
                this.ip += 4;
                break;
            }
            case eq: {
                const val = this.get(this.ip + 1, modes[0]) === this.get(this.ip + 2, modes[1]) ? 1 : 0;
                this.set(this.ip + 3, modes[2], val);
                this.ip += 4;
                break;
            }
            case rbo: {
                this.relbase += this.get(this.ip + 1, modes[0]);
                this.ip += 2;
                break;
            }
            case halt:
                return false;
            default:
                throw new Error(`Unknown opcode: ${op}`);
        }
        return true;
    }

    run() {
        while (this.step()) { }
        return this.output;
    }
}

const N = 0;
const E = 1;
const S = 2;
const W = 3;

const point = {
    [N]: [0, 1],
    [E]: [1, 0],
    [S]: [0, -1],
    [W]: [-1, 0],
};

const pointReversed = {
    [N]: [0, -1],
    [E]: [1, 0],
    [S]: [0, 1],
    [W]: [-1, 0],
};

function addPoints(p1, p2) {
    return [p1[0] + p2[0], p1[1] + p2[1]];
}

const fromPoint = {
    '0,1': N,
    '1,0': E,
    '0,-1': S,
    '-1,0': W,
};

function dirFromPoint(p) {
    return fromPoint[`${p[0]},${p[1]}`];
}

function nextDir(d) {
    return (d + 1) % 4;
}

function prevDir(d) {
    return (d + 3) % 4;
}

const fromByte = {
    'N': N,
    'E': E,
    'S': S,
    'W': W,
    'U': N,
    'R': E,
    'D': S,
    'L': W,
    '^': N,
    '>': E,
    'v': S,
    '<': W,
};

function dirFromByte(b) {
    return fromByte[b];
}

function readAll(filepath) {
    return fs.readFileSync(filepath, 'utf-8').trim();
}

function Atoi(s) {
    return parseInt(s, 10);
}

function parse(program) {
    const machine = new Machine(program, []);
    const output = machine.run();
    const sb = output.map(o => String.fromCharCode(o)).join('');
    const scaffolding = new Map();
    let robot = null;
    let dir = null;
    const lines = sb.split('\n');
    for (let y = 0; y < lines.length; y++) {
        const line = lines[y];
        for (let x = 0; x < line.length; x++) {
            const char = line[x];
            if (['^', 'v', '<', '>'].includes(char)) {
                robot = [x, y];
                dir = dirFromByte(char);
            }
            if (['#', '^', 'v', '<', '>'].includes(char)) {
                scaffolding.set(`${x},${y}`, true);
            }
        }
    }
    return [scaffolding, robot, dir];
}

function path(scaffolding, robot, dir) {
    let dist = 0;
    let d = null;
    const sections = [];
    while (true) {
        const nextPos = addPoints(robot, pointReversed[dir]);
        if (scaffolding.has(`${nextPos[0]},${nextPos[1]}`)) {
            robot = nextPos;
            dist++;
            continue;
        }
        if (dist > 0) {
            sections.push(`${d},${dist}`);
        }
        const nextPosNext = addPoints(robot, pointReversed[nextDir(dir)]);
        if (scaffolding.has(`${nextPosNext[0]},${nextPosNext[1]}`)) {
            robot = nextPosNext;
            dir = nextDir(dir);
            dist = 1;
            d = 'R';
        } else {
            const nextPosPrev = addPoints(robot, pointReversed[prevDir(dir)]);
            if (scaffolding.has(`${nextPosPrev[0]},${nextPosPrev[1]}`)) {
                robot = nextPosPrev;
                dir = prevDir(dir);
                dist = 1;
                d = 'L';
            } else {
                break;
            }
        }
    }
    return sections.join(',');
}

function encode(path) {
    let seq = null;
    let a = null;
    let b = null;
    let c = null;
    const pathWithComma = path + ',';
    loop:
    for (let i = 2; i <= 21; i++) {
        for (let j = 2; j <= 21; j++) {
            for (let k = 2; k <= 21; k++) {
                let next = pathWithComma;
                a = next.slice(0, i);
                next = next.split(a).join('');
                b = next.slice(0, j);
                next = next.split(b).join('');
                c = next.slice(0, k);
                next = next.split(c).join('');
                if (next === '') {
                    break loop;
                }
            }
        }
    }
    a = a.replace(/,$/, '');
    b = b.replace(/,$/, '');
    c = c.replace(/,$/, '');
    seq = path.split(a).join('A').split(b).join('B').split(c).join('C');
    return [seq, a, b, c];
}

function dust(program, scaffolding, robot, dir) {
    const [seq, a, b, c] = encode(path(scaffolding, robot, dir));
    const inputStr = `${seq}\n${a}\n${b}\n${c}\nn\n`;
    const input = inputStr.split('').map(char => char.charCodeAt(0));
    program[0] = 2;
    const machine = new Machine(program, input);
    const output = machine.run();
    return output[output.length - 1];
}

function main() {
    const program = readAll('input.txt').split(',').map(Atoi);
    const [scaffolding, robot, dir] = parse(program);
    console.log(dust(program, scaffolding, robot, dir));
}

main();
