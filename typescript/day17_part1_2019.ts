import * as fs from 'fs';

enum Mode {
    Position,
    Immediate,
    Relative,
}

enum Opcode {
    Add = 1,
    Mul,
    Input,
    Output,
    Jt,
    Jf,
    Lt,
    Eq,
    Rbo,
    Halt = 99,
}

function decode(n: number): [Opcode, Mode[]] {
    const op = n % 100 as Opcode;
    n = Math.floor(n / 100);
    const modes = [];
    for (let i = 0; i < 3; i++) {
        modes.push(n % 10 as Mode);
        n = Math.floor(n / 10);
    }
    return [op, modes];
}

class Machine {
    data: Map<number, number>;
    ip: number;
    inChannel: IterableIterator<number>;
    outChannel: number[];
    relbase: number;

    constructor(program: number[], inChannel: IterableIterator<number>, outChannel: number[]) {
        this.data = new Map<number, number>();
        this.ip = 0;
        this.inChannel = inChannel;
        this.outChannel = outChannel;
        this.relbase = 0;
        program.forEach((n, i) => this.data.set(i, n));
    }

    get(i: number, mo: Mode): number {
        switch (mo) {
            case Mode.Immediate: return this.data.get(i) || 0;
            case Mode.Position: return this.data.get(this.data.get(i) || 0) || 0;
            case Mode.Relative: return this.data.get(this.relbase + (this.data.get(i) || 0)) || 0;
            default: throw new Error(`Unknown mode: ${mo}`);
        }
    }

    set(i: number, mo: Mode, val: number): void {
        switch (mo) {
            case Mode.Position: this.data.set(this.data.get(i) || 0, val); break;
            case Mode.Relative: this.data.set(this.relbase + (this.data.get(i) || 0), val); break;
            default: throw new Error(`Unknown mode: ${mo}`);
        }
    }

    step(): boolean {
        const [op, modes] = decode(this.data.get(this.ip) || 0);
        switch (op) {
            case Opcode.Add:
                this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) + this.get(this.ip + 2, modes[1]));
                this.ip += 4; break;
            case Opcode.Mul:
                this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) * this.get(this.ip + 2, modes[1]));
                this.ip += 4; break;
            case Opcode.Input:
                this.set(this.ip + 1, modes[0], this.inChannel.next().value);
                this.ip += 2; break;
            case Opcode.Output:
                this.outChannel.push(this.get(this.ip + 1, modes[0]));
                this.ip += 2; break;
            case Opcode.Jt:
                this.ip = this.get(this.ip + 1, modes[0]) !== 0 ? this.get(this.ip + 2, modes[1]) : this.ip + 3; break;
            case Opcode.Jf:
                this.ip = this.get(this.ip + 1, modes[0]) === 0 ? this.get(this.ip + 2, modes[1]) : this.ip + 3; break;
            case Opcode.Lt:
                this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) < this.get(this.ip + 2, modes[1]) ? 1 : 0);
                this.ip += 4; break;
            case Opcode.Eq:
                this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) === this.get(this.ip + 2, modes[1]) ? 1 : 0);
                this.ip += 4; break;
            case Opcode.Rbo:
                this.relbase += this.get(this.ip + 1, modes[0]);
                this.ip += 2; break;
            case Opcode.Halt:
                return false;
            default:
                throw new Error(`Unknown opcode: ${op}`);
        }
        return true;
    }

    run(): void {
        while (this.step()) {}
    }
}

function run(program: number[], inChannel: IterableIterator<number>): number[] {
    const outChannel: number[] = [];
    const machine = new Machine(program, inChannel, outChannel);
    machine.run();
    return outChannel;
}

function readInput(filepath: string): number[] {
    const input = fs.readFileSync(filepath, 'utf-8').trim();
    return input.split(',').map(Number);
}

function parse(program: number[]): [Map<string, boolean>, [number, number], number] {
    const inChannel = (function* () { yield* []; })();
    const out = run(program, inChannel);
    const sb = out.map(o => String.fromCharCode(o)).join('');
    const scaffolding = new Map<string, boolean>();
    let robot: [number, number] = [0, 0];
    let dir = 0;
    sb.split('\n').forEach((line, y) => {
        for (let x = 0; x < line.length; x++) {
            if ('^v<>'.includes(line[x])) {
                robot = [x, y];
                dir = '^v<>'.indexOf(line[x]);
                scaffolding.set(`${x},${y}`, true);
            } else if (line[x] === '#') {
                scaffolding.set(`${x},${y}`, true);
            }
        }
    });
    return [scaffolding, robot, dir];
}

function sumAlign(grid: Map<string, boolean>): number {
    let sum = 0;
    grid.forEach((_, key) => {
        const [x, y] = key.split(',').map(Number);
        const neighbors = [
            `${x},${y + 1}`, `${x},${y - 1}`,
            `${x + 1},${y}`, `${x - 1},${y}`
        ];
        if (neighbors.every(n => grid.has(n))) {
            sum += x * y;
        }
    });
    return sum;
}

const program = readInput('input.txt');
const [scaffolding, robot, dir] = parse(program);
console.log(sumAlign(scaffolding));