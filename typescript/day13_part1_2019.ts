import * as fs from 'fs';

enum Mode { Position, Immediate, Relative }
enum Opcode { Add = 1, Mul, Input, Output, Jt, Jf, Lt, Eq, Rbo, Halt = 99 }

function decode(n: number): [Opcode, Mode[]] {
    const op = n % 100 as Opcode;
    n = Math.floor(n / 100);
    const modes: Mode[] = [];
    for (let i = 0; i < 3; i++) {
        modes.push(n % 10 as Mode);
        n = Math.floor(n / 10);
    }
    return [op, modes];
}

class Machine {
    data: Record<number, number>;
    ip: number;
    inChannel: IterableIterator<number>;
    outChannel: number[];
    relbase: number;

    constructor(program: number[], input: IterableIterator<number>) {
        this.data = {};
        program.forEach((n, i) => this.data[i] = n);
        this.ip = 0;
        this.inChannel = input;
        this.outChannel = [];
        this.relbase = 0;
    }

    get(i: number, mo: Mode): number {
        switch (mo) {
            case Mode.Immediate: return this.data[i];
            case Mode.Position: return this.data[this.data[i]] || 0;
            case Mode.Relative: return this.data[this.relbase + this.data[i]] || 0;
            default: throw new Error(`Unknown mode: ${mo}`);
        }
    }

    set(i: number, mo: Mode, val: number): void {
        switch (mo) {
            case Mode.Position: this.data[this.data[i]] = val; break;
            case Mode.Relative: this.data[this.relbase + this.data[i]] = val; break;
            default: throw new Error(`Unknown mode: ${mo}`);
        }
    }

    step(): boolean {
        const [op, modes] = decode(this.data[this.ip]);
        switch (op) {
            case Opcode.Add: this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) + this.get(this.ip + 2, modes[1])); this.ip += 4; break;
            case Opcode.Mul: this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) * this.get(this.ip + 2, modes[1])); this.ip += 4; break;
            case Opcode.Input: this.set(this.ip + 1, modes[0], this.inChannel.next().value); this.ip += 2; break;
            case Opcode.Output: this.outChannel.push(this.get(this.ip + 1, modes[0])); this.ip += 2; break;
            case Opcode.Jt: this.ip = this.get(this.ip + 1, modes[0]) !== 0 ? this.get(this.ip + 2, modes[1]) : this.ip + 3; break;
            case Opcode.Jf: this.ip = this.get(this.ip + 1, modes[0]) === 0 ? this.get(this.ip + 2, modes[1]) : this.ip + 3; break;
            case Opcode.Lt: this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) < this.get(this.ip + 2, modes[1]) ? 1 : 0); this.ip += 4; break;
            case Opcode.Eq: this.set(this.ip + 3, modes[2], this.get(this.ip + 1, modes[0]) === this.get(this.ip + 2, modes[1]) ? 1 : 0); this.ip += 4; break;
            case Opcode.Rbo: this.relbase += this.get(this.ip + 1, modes[0]); this.ip += 2; break;
            case Opcode.Halt: return false;
            default: throw new Error(`Unknown opcode: ${op}`);
        }
        return true;
    }

    run(): void {
        while (this.step());
    }
}

function run(program: number[], input: IterableIterator<number>): number[] {
    const machine = new Machine(program, input);
    machine.run();
    return machine.outChannel;
}

function countBlocks(program: number[]): number {
    const grid: Record<string, number> = {};
    const input = (function* () { })();
    const output = run(program, input);
    for (let i = 0; i < output.length; i += 3) {
        grid[`${output[i]},${output[i + 1]}`] = output[i + 2];
    }
    return Object.values(grid).filter(t => t === 2).length;
}

function readAll(filepath: string): string {
    return fs.readFileSync(filepath, 'utf-8').trim();
}

function toInt(s: string): number {
    return parseInt(s, 10);
}

const program = readAll('input.txt').split(',').map(toInt);
console.log(countBlocks(program));