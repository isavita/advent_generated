
import * as fs from 'fs';

type Mode = 0 | 1 | 2;

class VM {
    code: Map<number, number>;
    ip: number;
    relativeBase: number;
    input: number[];
    output: number[];

    constructor(code: number[]) {
        this.code = new Map(code.map((val, idx) => [idx, val]));
        this.ip = 0;
        this.relativeBase = 0;
        this.input = [];
        this.output = [];
    }

    load(filename: string) {
        const data = fs.readFileSync(filename, 'utf-8');
        const listStr = data.trim().split(',').map(Number);
        this.code = new Map(listStr.map((val, idx) => [idx, val]));
        this.ip = 0;
        this.relativeBase = 0;
    }

    getParamAddress(pos: number, mode: Mode): number {
        const value = this.code.get(pos) || 0;
        switch (mode) {
            case 0:
                return value;
            case 1:
                return pos;
            case 2:
                return this.relativeBase + value;
            default:
                throw new Error("Invalid mode");
        }
    }

    getParamsAddresses(pos: number, cmd: number, arity: number): number[] {
        const modes = this.getModes(cmd, arity);
        const params = [];
        for (let i = 0; i < arity; i++) {
            params.push(this.getParamAddress(pos + i + 1, modes[i]));
        }
        return params;
    }

    getOpCode(cmd: number): number {
        return cmd % 100;
    }

    getModes(cmd: number, arity: number): Mode[] {
        const modeSection = Math.floor(cmd / 100);
        const modes: Mode[] = [];
        for (let i = 0; i < arity; i++) {
            modes.push((Math.floor(modeSection / Math.pow(10, i)) % 10) as Mode);
        }
        return modes;
    }

    run(input: number[]): number[] {
        this.input = input;
        this.output = [];
        let inputIndex = 0;
        let arity: number;

        while (true) {
            const cmd = this.code.get(this.ip) || 0;
            const opCode = this.getOpCode(cmd);

            switch (opCode) {
                case 1:
                    arity = 3;
                    const params1 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code.set(params1[2], (this.code.get(params1[0]) || 0) + (this.code.get(params1[1]) || 0));
                    break;
                case 2:
                    arity = 3;
                    const params2 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code.set(params2[2], (this.code.get(params2[0]) || 0) * (this.code.get(params2[1]) || 0));
                    break;
                case 3:
                    arity = 1;
                    const params3 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code.set(params3[0], this.input[inputIndex++]);
                    break;
                case 4:
                    arity = 1;
                    const params4 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.output.push(this.code.get(params4[0]) || 0);
                    break;
                case 5:
                    arity = 2;
                    const params5 = this.getParamsAddresses(this.ip, cmd, arity);
                    if ((this.code.get(params5[0]) || 0) !== 0) {
                        this.ip = this.code.get(params5[1]) || 0;
                        continue;
                    }
                    break;
                case 6:
                    arity = 2;
                    const params6 = this.getParamsAddresses(this.ip, cmd, arity);
                    if ((this.code.get(params6[0]) || 0) === 0) {
                        this.ip = this.code.get(params6[1]) || 0;
                        continue;
                    }
                    break;
                case 7:
                    arity = 3;
                    const params7 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code.set(params7[2], (this.code.get(params7[0]) || 0) < (this.code.get(params7[1]) || 0) ? 1 : 0);
                    break;
                case 8:
                    arity = 3;
                    const params8 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code.set(params8[2], (this.code.get(params8[0]) || 0) === (this.code.get(params8[1]) || 0) ? 1 : 0);
                    break;
                case 9:
                    arity = 1;
                    const params9 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.relativeBase += (this.code.get(params9[0]) || 0);
                    break;
                case 99:
                    return this.output;
                default:
                    throw new Error(`Invalid opcode: ${opCode}`);
            }
            this.ip += arity + 1;
        }
    }
}

function beam(x: number, y: number, vm: VM): boolean {
    const output = vm.run([x, y]);
    return output[0] === 1;
}

function main() {
    const vm = new VM([]);
    vm.load("input.txt");

    let y = 20;
    let x = 0;

    while (true) {
        if (!beam(x, y, new VM([...vm.code.values()]))) {
            x++;
            continue;
        }

        if (!beam(x + 99, y, new VM([...vm.code.values()]))) {
            y++;
            continue;
        }

        if (!beam(x, y + 99, new VM([...vm.code.values()]))) {
            x++;
            continue;
        }

        console.log(x * 10000 + y);
        return;
    }
}

main();
