const fs = require('fs');

class VM {
    constructor() {
        this.code = null;
        this.ip = 0;
        this.relativeBase = 0;
        this.input = [];
        this.output = [];
    }

    load(filename) {
        const data = fs.readFileSync(filename, 'utf8').trim();
        this.code = data.split(',').map(Number);
        this.ip = 0;
        this.relativeBase = 0;
    }

    run() {
        let arity = 0;

        while (true) {
            const cmd = this.code[this.ip];

            switch (this.opCode(cmd)) {
                case 1:
                    arity = 3;
                    const [p1, p2, p3] = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code[p3] = this.code[p1] + this.code[p2];
                    break;

                case 2:
                    arity = 3;
                    const [p4, p5, p6] = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code[p6] = this.code[p4] * this.code[p5];
                    break;

                case 3:
                    arity = 1;
                    const [p7] = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code[p7] = this.input.shift();
                    break;

                case 4:
                    arity = 1;
                    const [p8] = this.getParamsAddresses(this.ip, cmd, arity);
                    this.output.push(this.code[p8]);
                    break;

                case 5:
                    arity = 2;
                    const [p9, p10] = this.getParamsAddresses(this.ip, cmd, arity);
                    if (this.code[p9] !== 0) {
                        this.ip = this.code[p10];
                        continue;
                    }
                    break;

                case 6:
                    arity = 2;
                    const [p11, p12] = this.getParamsAddresses(this.ip, cmd, arity);
                    if (this.code[p11] === 0) {
                        this.ip = this.code[p12];
                        continue;
                    }
                    break;

                case 7:
                    arity = 3;
                    const [p13, p14, p15] = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code[p15] = this.code[p13] < this.code[p14] ? 1 : 0;
                    break;

                case 8:
                    arity = 3;
                    const [p16, p17, p18] = this.getParamsAddresses(this.ip, cmd, arity);
                    this.code[p18] = this.code[p16] === this.code[p17] ? 1 : 0;
                    break;

                case 9:
                    arity = 1;
                    const [p19] = this.getParamsAddresses(this.ip, cmd, arity);
                    this.relativeBase += this.code[p19];
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

    opCode(cmd) {
        return cmd % 100;
    }

    getParamsAddresses(pos, cmd, arity) {
        const modes = this.modes(cmd, arity);
        const results = [];

        for (let i = 0; i < arity; i++) {
            results.push(this.getParamAddress(pos + i + 1, modes[i]));
        }

        return results;
    }

    modes(cmd, arity) {
        const modeSection = Math.floor(cmd / 100);
        const modes = [];

        for (let i = 0; i < arity; i++) {
            modes.push(Math.floor(modeSection / 10 ** i) % 10);
        }

        return modes;
    }

    getParamAddress(pos, mode) {
        switch (mode) {
            case 0:
                return this.code[pos];
            case 1:
                return pos;
            case 2:
                return this.relativeBase + this.code[pos];
            default:
                console.error('wrong mode');
                process.exit(1);
        }
    }
}

const vm = new VM();
vm.load('input.txt');

const instructions = [
    "NOT A J",
    "NOT B T",
    "OR T J",
    "NOT C T",
    "OR T J",
    "AND D J",
    "WALK"
];

instructions.forEach(instruction => {
    for (const char of instruction) {
        vm.input.push(char.charCodeAt(0));
    }
    vm.input.push(10); // newline
});

vm.run();

console.log(vm.output.pop());