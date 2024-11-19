
import 'dart:io';

enum PanelColor { black, white }

enum Direction { up, right, down, left }

class Position {
  int x, y;
  Position(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      other is Position && other.x == x && other.y == y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

class Robot {
  Position position;
  Direction direction;
  Robot(this.position, this.direction);

  void turnAndMove(int turnDirection) {
    if (turnDirection == 0) {
      direction = Direction.values[(direction.index + 3) % 4];
    } else {
      direction = Direction.values[(direction.index + 1) % 4];
    }

    switch (direction) {
      case Direction.up:
        position = Position(position.x, position.y - 1);
        break;
      case Direction.right:
        position = Position(position.x + 1, position.y);
        break;
      case Direction.down:
        position = Position(position.x, position.y + 1);
        break;
      case Direction.left:
        position = Position(position.x - 1, position.y);
        break;
    }
  }
}

class Intcode {
  List<int> memory;
  int ip = 0;
  List<int> input = [];
  List<int> output = [];
  bool halted = false;

  Intcode(this.memory);

  void addInput(int input) => this.input.add(input);

  void run() {
    output = [];
    while (true) {
      int opcode = memory[ip] % 100;
      switch (opcode) {
        case 1:
        case 2:
        case 7:
        case 8:
          ensureMemory(ip + 3);
          List<int> params = getParams(3);
          int val1 = readMemory(params[0]), val2 = readMemory(params[1]);
          if (opcode == 1) {
            writeMemory(params[2], val1 + val2);
          } else if (opcode == 2) {
            writeMemory(params[2], val1 * val2);
          } else if (opcode == 7 && val1 < val2 || opcode == 8 && val1 == val2) {
            writeMemory(params[2], 1);
          } else {
            writeMemory(params[2], 0);
          }
          ip += 4;
          break;
        case 3:
        case 4:
          ensureMemory(ip + 1);
          List<int> params = getParams(1);
          if (opcode == 3) {
            if (input.isEmpty) return;
            writeMemory(params[0], input.removeAt(0));
          } else {
            output.add(readMemory(params[0]));
          }
          ip += 2;
          break;
        case 5:
        case 6:
          ensureMemory(ip + 2);
          List<int> params = getParams(2);
          int val = readMemory(params[0]), target = readMemory(params[1]);
          if ((opcode == 5 && val != 0) || (opcode == 6 && val == 0)) {
            ip = target;
          } else {
            ip += 3;
          }
          break;
        case 99:
          halted = true;
          return;
        default:
          throw FormatException('unknown opcode: $opcode');
      }
    }
  }

  int readMemory(int address) {
    ensureMemory(address);
    return memory[address];
  }

  void writeMemory(int address, int value) {
    ensureMemory(address);
    memory[address] = value;
  }

  void ensureMemory(int address) {
    if (address >= memory.length) {
      memory = List<int>.filled(address + 1, 0)..setRange(0, memory.length, memory);
    }
  }

  List<int> getParams(int count) {
    int paramModes = memory[ip] ~/ 100;
    List<int> params = List<int>.generate(count, (i) {
      int mode = paramModes % 10;
      paramModes ~/= 10;
      return mode == 1 ? ip + i + 1 : memory[ip + i + 1];
    });
    return params;
  }
}

void main() async {
  List<String> codeStr = await File('input.txt').readAsString().then((s) => s.trim().split(','));
  List<int> program = codeStr.map(int.parse).toList();

  Map<Position, PanelColor> grid = {};
  Robot robot = Robot(Position(0, 0), Direction.up);
  Intcode intcode = Intcode(program);

  while (!intcode.halted) {
    PanelColor currentColor = grid[robot.position] ?? PanelColor.black;
    intcode.addInput(currentColor.index);
    intcode.run();
    List<int> outputs = intcode.output;

    if (outputs.length == 2) {
      grid[robot.position] = PanelColor.values[outputs[0]];
      robot.turnAndMove(outputs[1]);
    }
  }

  print(grid.length);
}
