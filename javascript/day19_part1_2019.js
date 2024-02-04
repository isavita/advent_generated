const fs = require('fs');

class VM {
    constructor() {
        this.code = null;
        this.ip = 0;
        this.input = [];
        this.output = [];
        this.relativeBase = 0;
    }

    load(filename) {
        const data = fs.readFileSync(filename, 'utf8').trim();
        this.code = data.split(',').map(Number);
        this.ip = 0;
        this.relativeBase = 0;
    }

    run() {
        let arity;

        while (true) {
            const cmd = this.code[this.ip];

            switch (this.opCode(cmd)) {
                case 1:
                    arity = 3;
                    const params1 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code[params1[2]] = this.code[params1[0]] + this.code[params1[1]];
                    break;

                case 2:
                    arity = 3;
                    const params2 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code[params2[2]] = this.code[params2[0]] * this.code[params2[1]];
                    break;

                case 3:
                    arity = 1;
                    const params3 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code[params3[0]] = this.input.shift();
                    break;

                case 4:
                    arity = 1;
                    const params4 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.output.push(this.code[params4[0]]);
                    break;

                case 5:
                    arity = 2;
                    const params5 = this.getParamsAddresses(this.ip, cmd, arity);
                    if (this.code[params5[0]] !== 0) {
                        this.ip = this.code[params5[1]];
                        continue;
                    }
                    break;

                case 6:
                    arity = 2;
                    const params6 = this.getParamsAddresses(this.ip, cmd, arity);
                    if (this.code[params6[0]] === 0) {
                        this.ip = this.code[params6[1]];
                        continue;
                    }
                    break;

                case 7:
                    arity = 3;
                    const params7 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code[params7[2]] = this.code[params7[0]] < this.code[params7[1]] ? 1 : 0;
                    break;

                case 8:
                    arity = 3;
                    const params8 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code[params8[2]] = this.code[params8[0]] === this.code[params8[1]] ? 1 : 0;
                    break;

                case 9:
                    arity = 1;
                    const params9 = this.getParamsAddresses(this.ip, cmd, arity);
                    this.relativeBase += this.code[params9[0]];
                    break;

                case 99:
                    return;

                default:
                    console.error(`not an opcode ${cmd}`);
                    process.exit(1);
            }

            this.ip += arity + 1;
        }
    }

    getParamsAddresses(pos, cmd, arity) {
        const modes = this.modes(cmd, arity);
        const results = [];
        for (let i = 0; i < arity; i++) {
            results.push(this.getParamAddress(pos + i + 1, modes[i]));
        }
        return results;
    }

    getParamAddress(pos, mode) {
        switch (mode) {
            case 0:
                return this.code[pos];
            case 1:
                return pos;
            case 2:
                return this.relativeBase + this.code[pos];
        }
        console.error('wrong mode');
        process.exit(1);
    }

    opCode(cmd) {
        return cmd % 100;
    }

    modes(cmd, arity) {
        const modeSection = Math.floor(cmd / 100);
        const modes = [];
        for (let i = 0; i < arity; i++) {
            modes.push(Math.floor(modeSection / 10 ** i) % 10);
        }
        return modes;
    }
}

function main() {
    const vm = new VM();
    vm.load('input.txt');

    let sum = 0;
    for (let y = 0; y < 50; y++) {
        for (let x = 0; x < 50; x++) {
            if (beam(x, y)) {
                sum++;
            }
        }
    }

    console.log(sum);
}

function beam(x, y) {
    const vm = new VM();
    vm.load('input.txt');

    vm.input.push(x);
    vm.input.push(y);

    vm.run();

    return vm.output.pop() === 1;
}

main();