
const fs = require('fs');

class VM {
    constructor(filename) {
        this.code = {};
        this.ip = 0;
        this.input = [];
        this.output = [];
        this.relativeBase = 0;
        this.load(filename);
    }

    load(filename) {
        const data = fs.readFileSync(filename, 'utf8').trim().split(',');
        data.forEach((value, index) => {
            this.code[index] = parseInt(value, 10);
        });
    }

    run() {
        while (true) {
            const cmd = this.code[this.ip] % 100;
            const modes = [];
            for (let i = 0; i < 3; i++) {
                modes.push(Math.floor(this.code[this.ip] / Math.pow(10, i + 2)) % 10);
            }

            switch (cmd) {
                case 1:
                    this.code[this.getParamAddress(3, modes[2])] = this.getParamValue(1, modes[0]) + this.getParamValue(2, modes[1]);
                    this.ip += 4;
                    break;
                case 2:
                    this.code[this.getParamAddress(3, modes[2])] = this.getParamValue(1, modes[0]) * this.getParamValue(2, modes[1]);
                    this.ip += 4;
                    break;
                case 3:
                    this.code[this.getParamAddress(1, modes[0])] = this.input.shift();
                    this.ip += 2;
                    break;
                case 4:
                    this.output.push(this.getParamValue(1, modes[0]));
                    this.ip += 2;
                    break;
                case 5:
                    if (this.getParamValue(1, modes[0]) !== 0) {
                        this.ip = this.getParamValue(2, modes[1]);
                    } else {
                        this.ip += 3;
                    }
                    break;
                case 6:
                    if (this.getParamValue(1, modes[0]) === 0) {
                        this.ip = this.getParamValue(2, modes[1]);
                    } else {
                        this.ip += 3;
                    }
                    break;
                case 7:
                    this.code[this.getParamAddress(3, modes[2])] = this.getParamValue(1, modes[0]) < this.getParamValue(2, modes[1]) ? 1 : 0;
                    this.ip += 4;
                    break;
                case 8:
                    this.code[this.getParamAddress(3, modes[2])] = this.getParamValue(1, modes[0]) === this.getParamValue(2, modes[1]) ? 1 : 0;
                    this.ip += 4;
                    break;
                case 9:
                    this.relativeBase += this.getParamValue(1, modes[0]);
                    this.ip += 2;
                    break;
                case 99:
                    return;
                default:
                    throw new Error(`Unknown opcode ${cmd}`);
            }
        }
    }

    getParamValue(offset, mode) {
        return mode === 1 ? this.code[this.ip + offset] : this.code[this.getParamAddress(offset, mode)];
    }

    getParamAddress(offset, mode) {
        switch (mode) {
            case 0:
                return this.code[this.ip + offset];
            case 1:
                return this.ip + offset;
            case 2:
                return this.relativeBase + this.code[this.ip + offset];
            default:
                throw new Error(`Unknown mode ${mode}`);
        }
    }
}

function sendString(vm, s) {
    for (const char of s) {
        vm.input.push(char.charCodeAt(0));
    }
    vm.input.push('\n'.charCodeAt(0));
}

function reader(vm) {
    return new Promise(resolve => {
        const interval = setInterval(() => {
            const output = vm.output.shift();
            if (output !== undefined) {
                if (output > 127) {
                    console.log(output);
                    clearInterval(interval);
                    resolve();
                }
            }
        }, 10);
    });
}

(async () => {
    const vm = new VM('input.txt');
    const instructions = [
        "NOT A J", // J = ¬A
        "NOT B T", // T = ¬B
        "OR T J",  // J = ¬A ∨ ¬B
        "NOT C T", // T = ¬C
        "OR T J",  // J = ¬A ∨ ¬B ∨ ¬C
        "AND D J", // J = ( ¬A ∨ ¬B ∨ ¬C ) ∧ D
        "NOT A T", // T = ?
        "AND A T", // T = false
        "OR E T",  // T = false ∨ E = E
        "OR H T",  // T = E ∨ H
        "AND T J", // J = ( ¬A ∨ ¬B ∨ ¬C ) ∧ D ∧ (E ∨ H)
        "RUN",
    ];

    for (const i of instructions) {
        sendString(vm, i);
    }

    vm.run();
    await reader(vm);
})();
