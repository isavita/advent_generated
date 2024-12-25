
import * as fs from 'fs';

enum PanelColor {
  Black = 0,
  White = 1,
}

enum Direction {
  Up = 0,
  Right = 1,
  Down = 2,
  Left = 3,
}

type Position = {
  x: number;
  y: number;
};

class Robot {
  position: Position;
  direction: Direction;

  constructor(position: Position, direction: Direction) {
    this.position = position;
    this.direction = direction;
  }

  turnAndMove(turnDirection: number) {
    this.direction =
      turnDirection === 0
        ? (this.direction + 3) % 4
        : (this.direction + 1) % 4;

    switch (this.direction) {
      case Direction.Up:
        this.position.y--;
        break;
      case Direction.Right:
        this.position.x++;
        break;
      case Direction.Down:
        this.position.y++;
        break;
      case Direction.Left:
        this.position.x--;
        break;
    }
  }
}

class Intcode {
  memory: Map<number, number>;
  ip: number;
  input: number[];
  output: number[];
  halted: boolean;
  relativeBase: number;

  constructor(program: number[]) {
    this.memory = new Map();
    program.forEach((val, i) => this.memory.set(i, val));
    this.ip = 0;
    this.input = [];
    this.output = [];
    this.halted = false;
    this.relativeBase = 0;
  }

  addInput(input: number) {
    this.input.push(input);
  }

  run() {
    this.output = [];
    while (true) {
      const opcode = this.readMemory(this.ip) % 100;
      switch (opcode) {
        case 1:
        case 2:
        case 7:
        case 8: {
          const params = this.getParams(3);
          const val1 = this.readMemory(params[0]);
          const val2 = this.readMemory(params[1]);
          if (opcode === 1) {
            this.writeMemory(params[2], val1 + val2);
          } else if (opcode === 2) {
            this.writeMemory(params[2], val1 * val2);
          } else if ((opcode === 7 && val1 < val2) || (opcode === 8 && val1 === val2)) {
            this.writeMemory(params[2], 1);
          } else {
            this.writeMemory(params[2], 0);
          }
          this.ip += 4;
          break;
        }
        case 3:
        case 4: {
          const params = this.getParams(1);
          if (opcode === 3) {
            if (this.input.length === 0) {
              return;
            }
            this.writeMemory(params[0], this.input.shift()!);
          } else {
            this.output.push(this.readMemory(params[0]));
          }
          this.ip += 2;
          break;
        }
        case 5:
        case 6: {
          const params = this.getParams(2);
          const val = this.readMemory(params[0]);
          const target = this.readMemory(params[1]);
          if ((opcode === 5 && val !== 0) || (opcode === 6 && val === 0)) {
            this.ip = target;
          } else {
            this.ip += 3;
          }
          break;
        }
        case 9: {
          const params = this.getParams(1);
          this.relativeBase += this.readMemory(params[0]);
          this.ip += 2;
          break;
        }
        case 99:
          this.halted = true;
          return;
        default:
          throw new Error(`Unknown opcode: ${opcode}`);
      }
    }
  }

  readMemory(address: number): number {
    return this.memory.get(address) || 0;
  }

  writeMemory(address: number, value: number) {
    this.memory.set(address, value);
  }

  getParams(count: number): number[] {
    const paramModes = Math.floor(this.readMemory(this.ip) / 100);
    const params: number[] = [];
    for (let i = 0; i < count; i++) {
      const mode = Math.floor(paramModes / Math.pow(10, i)) % 10;
      if (mode === 0) {
        params.push(this.readMemory(this.ip + i + 1));
      } else if (mode === 1) {
        params.push(this.ip + i + 1);
      } else {
        params.push(this.readMemory(this.ip + i + 1) + this.relativeBase);
      }
    }
    return params;
  }

  outputs(): number[] {
    return this.output;
  }

  isHalted(): boolean {
    return this.halted;
  }
}

const data = fs.readFileSync('input.txt', 'utf-8');
const codeStr = data.trim().split(',');
const program = codeStr.map(Number);

const grid = new Map<string, PanelColor>();
const robot = new Robot({ x: 0, y: 0 }, Direction.Up);
const intcode = new Intcode(program);

while (!intcode.isHalted()) {
  const currentColor = grid.get(`${robot.position.x},${robot.position.y}`) || PanelColor.Black;
  intcode.addInput(currentColor);
  intcode.run();
  const outputs = intcode.outputs();

  if (outputs.length === 2) {
    grid.set(`${robot.position.x},${robot.position.y}`, outputs[0]);
    robot.turnAndMove(outputs[1]);
  }
}

console.log(grid.size);
