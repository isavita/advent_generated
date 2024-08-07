class VM {
  code: Map<number, number>;
  ip: number;
  input: number[];
  output: number[];
  relativeBase: number;

  constructor() {
    this.code = new Map();
    this.ip = 0;
    this.input = [];
    this.output = [];
    this.relativeBase = 0;
  }

  load(filename: string) {
    const fs = require('fs');
    const data = fs.readFileSync(filename, 'utf8');
    const listStr = data.split(',');
    for (let i = 0; i < listStr.length; i++) {
      this.code.set(i, parseInt(listStr[i]));
    }
  }

  run() {
    let arity: number;
    while (true) {
      const cmd = this.code.get(this.ip);
      if (cmd === undefined) {
        throw new Error('undefined opcode');
      }
      switch (this.opCode(cmd)) {
        case 1:
          arity = 3;
          const params = this.getParamsAddresses(this.ip, cmd, arity);
          this.code.set(params[2], (this.code.get(params[0]) || 0) + (this.code.get(params[1]) || 0));
          break;
        case 2:
          arity = 3;
          const params2 = this.getParamsAddresses(this.ip, cmd, arity);
          this.code.set(params2[2], (this.code.get(params2[0]) || 0) * (this.code.get(params2[1]) || 0));
          break;
        case 3:
          arity = 1;
          const params3 = this.getParamsAddresses(this.ip, cmd, arity);
          this.code.set(params3[0], this.input.shift() || 0);
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
          this.relativeBase += this.code.get(params9[0]) || 0;
          break;
        case 99:
          return;
        default:
          throw new Error(`not an opcode ${cmd}`);
      }
      this.ip += arity + 1;
    }
  }

  getParamsAddresses(pos: number, cmd: number, arity: number): number[] {
    const modes = this.modes(cmd, arity);
    const results = new Array(arity);
    for (let i = 0; i < arity; i++) {
      results[i] = this.getParamAddress(pos + i + 1, modes[i]);
    }
    return results;
  }

  getParamAddress(pos: number, mode: number): number {
    switch (mode) {
      case 0:
        return this.code.get(pos) || 0;
      case 1:
        return pos;
      case 2:
        return this.relativeBase + (this.code.get(pos) || 0);
      default:
        throw new Error(`wrong mode ${mode}`);
    }
  }

  opCode(cmd: number): number {
    return cmd % 100;
  }

  modes(cmd: number, arity: number): number[] {
    const modeSection = Math.floor(cmd / 100);
    const modes = new Array(arity);
    for (let i = 0; i < arity; i++) {
      modes[i] = Math.floor(modeSection / Math.pow(10, i)) % 10;
    }
    return modes;
  }
}

function beam(x: number, y: number): boolean {
  const vm = new VM();
  vm.load('input.txt');
  vm.input.push(x);
  vm.input.push(y);
  vm.run();
  return (vm.output[0] || 0) === 1;
}

function main() {
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

main();