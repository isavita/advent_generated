class VM {
  code: { [key: number]: number };
  ip: number;
  input: number[] = [];
  output: number[] = [];
  relativeBase: number = 0;

  constructor(code: number[]) {
    this.code = {};
    for (let i = 0; i < code.length; i++) {
      this.code[i] = code[i];
    }
    this.ip = 0;
  }

  load(filename: string) {
    const fs = require('fs');
    const data = fs.readFileSync(filename, 'utf8');
    const code = data.split(',').map(Number);
    this.code = {};
    for (let i = 0; i < code.length; i++) {
      this.code[i] = code[i];
    }
    this.ip = 0;
    this.relativeBase = 0;
  }

  run() {
    while (true) {
      const cmd = this.code[this.ip];
      switch (this.opCode(cmd)) {
        case 1:
          this.add();
          break;
        case 2:
          this.multiply();
          break;
        case 3:
          this.read();
          break;
        case 4:
          this.write();
          break;
        case 5:
          this.jumpNotZero();
          break;
        case 6:
          this.jumpZero();
          break;
        case 7:
          this.lessThan();
          break;
        case 8:
          this.equal();
          break;
        case 9:
          this.changeRelativeBase();
          break;
        case 99:
          return;
        default:
          throw new Error(`not an opcode ${cmd}`);
      }
    }
  }

  opCode(cmd: number) {
    return cmd % 100;
  }

  modes(cmd: number, arity: number) {
    const modeSection = Math.floor(cmd / 100);
    const modes = [];
    for (let i = 0; i < arity; i++) {
      modes.push(Math.floor(modeSection / Math.pow(10, i)) % 10);
    }
    return modes;
  }

  getParamAddress(pos: number, mode: number) {
    switch (mode) {
      case 0:
        return this.code[pos] ?? 0;
      case 1:
        return pos;
      case 2:
        return this.relativeBase + (this.code[pos] ?? 0);
      default:
        throw new Error('wrong mode');
    }
  }

  add() {
    const modes = this.modes(this.code[this.ip], 3);
    const params = [this.getParamAddress(this.ip + 1, modes[0]), this.getParamAddress(this.ip + 2, modes[1]), this.getParamAddress(this.ip + 3, modes[2])];
    this.code[params[2]] = (this.code[params[0]] ?? 0) + (this.code[params[1]] ?? 0);
    this.ip += 4;
  }

  multiply() {
    const modes = this.modes(this.code[this.ip], 3);
    const params = [this.getParamAddress(this.ip + 1, modes[0]), this.getParamAddress(this.ip + 2, modes[1]), this.getParamAddress(this.ip + 3, modes[2])];
    this.code[params[2]] = (this.code[params[0]] ?? 0) * (this.code[params[1]] ?? 0);
    this.ip += 4;
  }

  read() {
    const modes = this.modes(this.code[this.ip], 1);
    const param = this.getParamAddress(this.ip + 1, modes[0]);
    this.code[param] = this.input.shift() ?? 0;
    this.ip += 2;
  }

  write() {
    const modes = this.modes(this.code[this.ip], 1);
    const param = this.getParamAddress(this.ip + 1, modes[0]);
    this.output.push(this.code[param] ?? 0);
    this.ip += 2;
  }

  jumpNotZero() {
    const modes = this.modes(this.code[this.ip], 2);
    const params = [this.getParamAddress(this.ip + 1, modes[0]), this.getParamAddress(this.ip + 2, modes[1])];
    if ((this.code[params[0]] ?? 0) !== 0) {
      this.ip = this.code[params[1]] ?? 0;
    } else {
      this.ip += 3;
    }
  }

  jumpZero() {
    const modes = this.modes(this.code[this.ip], 2);
    const params = [this.getParamAddress(this.ip + 1, modes[0]), this.getParamAddress(this.ip + 2, modes[1])];
    if ((this.code[params[0]] ?? 0) === 0) {
      this.ip = this.code[params[1]] ?? 0;
    } else {
      this.ip += 3;
    }
  }

  lessThan() {
    const modes = this.modes(this.code[this.ip], 3);
    const params = [this.getParamAddress(this.ip + 1, modes[0]), this.getParamAddress(this.ip + 2, modes[1]), this.getParamAddress(this.ip + 3, modes[2])];
    if ((this.code[params[0]] ?? 0) < (this.code[params[1]] ?? 0)) {
      this.code[params[2]] = 1;
    } else {
      this.code[params[2]] = 0;
    }
    this.ip += 4;
  }

  equal() {
    const modes = this.modes(this.code[this.ip], 3);
    const params = [this.getParamAddress(this.ip + 1, modes[0]), this.getParamAddress(this.ip + 2, modes[1]), this.getParamAddress(this.ip + 3, modes[2])];
    if ((this.code[params[0]] ?? 0) === (this.code[params[1]] ?? 0)) {
      this.code[params[2]] = 1;
    } else {
      this.code[params[2]] = 0;
    }
    this.ip += 4;
  }

  changeRelativeBase() {
    const modes = this.modes(this.code[this.ip], 1);
    const param = this.getParamAddress(this.ip + 1, modes[0]);
    this.relativeBase += this.code[param] ?? 0;
    this.ip += 2;
  }
}

function sendString(input: number[], s: string) {
  for (let i = 0; i < s.length; i++) {
    input.push(s.charCodeAt(i));
  }
  input.push(10); // newline
}

function main() {
  const fs = require('fs');
  const data = fs.readFileSync('input.txt', 'utf8');
  const code = data.split(',').map(Number);
  const vm = new VM(code);
  const instructions = [
    'NOT A J',
    'NOT B T',
    'OR T J',
    'NOT C T',
    'OR T J',
    'AND D J',
    'WALK',
  ];
  for (const instruction of instructions) {
    sendString(vm.input, instruction);
  }
  vm.run();
  console.log(vm.output);
}

main();