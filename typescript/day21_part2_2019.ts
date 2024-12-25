
import * as fs from 'fs';

class VM {
  code: Map<number, number>;
  ip: number;
  input: number[];
  output: number[];
  relativeBase: number;
  inputIndex: number;

  constructor(filename: string) {
    this.code = new Map();
    this.ip = 0;
    this.input = [];
    this.output = [];
    this.relativeBase = 0;
    this.inputIndex = 0;
    this.load(filename);
  }

  load(filename: string) {
    const buf = fs.readFileSync(filename, 'utf-8');
    const listStr = buf.trim().split(',');
    listStr.forEach((s, i) => this.code.set(i, parseInt(s)));
  }

  run() {
    let arity: number;

    while (true) {
      const cmd = this.code.get(this.ip)!;
      const opCode = cmd % 100;

      switch (opCode) {
        case 1:
          arity = 3;
          this.opAdd(cmd, arity);
          break;
        case 2:
          arity = 3;
          this.opMultiply(cmd, arity);
          break;
        case 3:
          arity = 1;
          this.opRead(cmd, arity);
          break;
        case 4:
          arity = 1;
          this.opWrite(cmd, arity);
          break;
        case 5:
          arity = 2;
          this.opJumpNotZero(cmd, arity);
          continue;
        case 6:
          arity = 2;
          this.opJumpZero(cmd, arity);
          continue;
        case 7:
          arity = 3;
          this.opLessThan(cmd, arity);
          break;
        case 8:
          arity = 3;
          this.opEqual(cmd, arity);
          break;
        case 9:
          arity = 1;
          this.opChangeRelativeBase(cmd, arity);
          break;
        case 99:
          return;
        default:
          throw new Error(`not an opcode ${cmd}`);
      }

      this.ip += arity + 1;
    }
  }

  private opAdd(cmd: number, arity: number) {
    const params = this.getParamsAddresses(this.ip, cmd, arity);
    this.code.set(params[2], this.code.get(params[0])! + this.code.get(params[1])!);
  }

  private opMultiply(cmd: number, arity: number) {
    const params = this.getParamsAddresses(this.ip, cmd, arity);
    this.code.set(params[2], this.code.get(params[0])! * this.code.get(params[1])!);
  }

  private opRead(cmd: number, arity: number) {
    const params = this.getParamsAddresses(this.ip, cmd, arity);
    if (this.inputIndex >= this.input.length) {
      throw new Error("Not enough input");
    }
    this.code.set(params[0], this.input[this.inputIndex++]);
  }

  private opWrite(cmd: number, arity: number) {
    const params = this.getParamsAddresses(this.ip, cmd, arity);
    this.output.push(this.code.get(params[0])!);
  }

  private opJumpNotZero(cmd: number, arity: number) {
    const params = this.getParamsAddresses(this.ip, cmd, arity);
    if (this.code.get(params[0])! !== 0) {
      this.ip = this.code.get(params[1])!;
    } else {
      this.ip += arity + 1;
    }
  }

  private opJumpZero(cmd: number, arity: number) {
    const params = this.getParamsAddresses(this.ip, cmd, arity);
    if (this.code.get(params[0])! === 0) {
      this.ip = this.code.get(params[1])!;
    } else {
      this.ip += arity + 1;
    }
  }

  private opLessThan(cmd: number, arity: number) {
    const params = this.getParamsAddresses(this.ip, cmd, arity);
    this.code.set(params[2], this.code.get(params[0])! < this.code.get(params[1])! ? 1 : 0);
  }

  private opEqual(cmd: number, arity: number) {
    const params = this.getParamsAddresses(this.ip, cmd, arity);
    this.code.set(params[2], this.code.get(params[0])! === this.code.get(params[1])! ? 1 : 0);
  }

  private opChangeRelativeBase(cmd: number, arity: number) {
    const params = this.getParamsAddresses(this.ip, cmd, arity);
    this.relativeBase += this.code.get(params[0])!;
  }

  private getParamsAddresses(pos: number, cmd: number, arity: number): number[] {
    const modes = this.getModes(cmd, arity);
    const results: number[] = [];
    for (let i = 0; i < arity; i++) {
      results.push(this.getParamAddress(pos + i + 1, modes[i]));
    }
    return results;
  }

  private getParamAddress(pos: number, mode: number): number {
    switch (mode) {
      case 0:
        return this.code.get(pos)!;
      case 1:
        return pos;
      case 2:
        return this.relativeBase + this.code.get(pos)!;
      default:
        throw new Error('wrong mode');
    }
  }

  private getModes(cmd: number, arity: number): number[] {
    const modeSection = Math.floor(cmd / 100);
    const modes: number[] = [];
    for (let i = 0; i < arity; i++) {
      modes.push(Math.floor(modeSection / Math.pow(10, i)) % 10);
    }
    return modes;
  }
}

function sendString(input: number[], s: string) {
  for (let i = 0; i < s.length; i++) {
    input.push(s.charCodeAt(i));
  }
  input.push('\n'.charCodeAt(0));
}

const vm = new VM('input.txt');

const instructions = [
  "NOT A J",
  "NOT B T",
  "OR T J",
  "NOT C T",
  "OR T J",
  "AND D J",
  "NOT A T",
  "AND A T",
  "OR E T",
  "OR H T",
  "AND T J",
  "RUN",
];

for (const i of instructions) {
  sendString(vm.input, i);
}

vm.run();

console.log(vm.output[vm.output.length - 1]);
