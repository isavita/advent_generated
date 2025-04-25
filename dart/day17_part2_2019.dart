
import 'dart:io';
import 'dart:math';

enum Mode { position, immediate, relative }
enum Opcode {
  add(1),
  mul(2),
  input(3),
  output(4),
  jt(5),
  jf(6),
  lt(7),
  eq(8),
  rbo(9),
  halt(99);

  final int value;
  const Opcode(this.value);

  static Opcode fromInt(int val) {
    return Opcode.values.firstWhere((e) => e.value == val,
        orElse: () => throw Exception("Unknown opcode: $val"));
  }
}

class Instruction {
  final Opcode opcode;
  final List<Mode> modes;

  Instruction(int value)
      : opcode = Opcode.fromInt(value % 100),
        modes = [
          Mode.values[(value ~/ 100) % 10],
          Mode.values[(value ~/ 1000) % 10],
          Mode.values[(value ~/ 10000) % 10]
        ];
}

class Machine {
  Map<int, int> data;
  int ip = 0;
  int relbase = 0;
  List<int> inputQueue;
  List<int> outputQueue = [];

  Machine(List<int> program, this.inputQueue)
      : data = {for (var i = 0; i < program.length; i++) i: program[i]};

  int _get(int index, Mode mode) {
    int value = data[ip + index] ?? 0;
    switch (mode) {
      case Mode.immediate:
        return value;
      case Mode.position:
        return data[value] ?? 0;
      case Mode.relative:
        return data[relbase + value] ?? 0;
    }
  }

  void _set(int index, Mode mode, int val) {
    int addr = data[ip + index] ?? 0;
    switch (mode) {
      case Mode.position:
        data[addr] = val;
        break;
      case Mode.relative:
        data[relbase + addr] = val;
        break;
      case Mode.immediate: // Should not happen for set operations
        throw Exception("Immediate mode not supported for set");
    }
  }

  bool step() {
    Instruction instruction = Instruction(data[ip] ?? 0);
    switch (instruction.opcode) {
      case Opcode.add:
        int val = _get(1, instruction.modes[0]) + _get(2, instruction.modes[1]);
        _set(3, instruction.modes[2], val);
        ip += 4;
        break;
      case Opcode.mul:
        int val = _get(1, instruction.modes[0]) * _get(2, instruction.modes[1]);
        _set(3, instruction.modes[2], val);
        ip += 4;
        break;
      case Opcode.input:
        if (inputQueue.isEmpty) {
           throw Exception("Input queue empty");
        }
        _set(1, instruction.modes[0], inputQueue.removeAt(0));
        ip += 2;
        break;
      case Opcode.output:
        outputQueue.add(_get(1, instruction.modes[0]));
        ip += 2;
        break;
      case Opcode.jt:
        if (_get(1, instruction.modes[0]) != 0) {
          ip = _get(2, instruction.modes[1]);
        } else {
          ip += 3;
        }
        break;
      case Opcode.jf:
        if (_get(1, instruction.modes[0]) == 0) {
          ip = _get(2, instruction.modes[1]);
        } else {
          ip += 3;
        }
        break;
      case Opcode.lt:
        _set(3, instruction.modes[2],
            _get(1, instruction.modes[0]) < _get(2, instruction.modes[1]) ? 1 : 0);
        ip += 4;
        break;
      case Opcode.eq:
        _set(3, instruction.modes[2],
            _get(1, instruction.modes[0]) == _get(2, instruction.modes[1]) ? 1 : 0);
        ip += 4;
        break;
      case Opcode.rbo:
        relbase += _get(1, instruction.modes[0]);
        ip += 2;
        break;
      case Opcode.halt:
        return false;
    }
    return true;
  }

  List<int> run() {
    while (step()) {}
    return outputQueue;
  }
}

enum Dir { N, E, S, W }

extension DirExt on Dir {
  Point<int> get point {
    switch (this) {
      case Dir.N: return Point(0, -1); // Up is negative Y
      case Dir.E: return Point(1, 0);
      case Dir.S: return Point(0, 1);  // Down is positive Y
      case Dir.W: return Point(-1, 0);
    }
  }

  Dir next() => Dir.values[(index + 1) % 4];
  Dir prev() => Dir.values[(index + 3) % 4];

  static Dir fromByte(int byte) {
    switch (String.fromCharCode(byte)) {
      case '^': return Dir.N;
      case '>': return Dir.E;
      case 'v': return Dir.S;
      case '<': return Dir.W;
      default: throw Exception('Invalid direction byte: $byte');
    }
  }
}

extension PointAdd on Point<int> {
  Point<int> operator +(Point<int> other) {
    return Point(x + other.x, y + other.y);
  }
}

(Set<Point<int>>, Point<int>, Dir) parse(List<int> program) {
  Machine machine = Machine(List.from(program), []);
  List<int> output = machine.run();
  String mapStr = String.fromCharCodes(output);

  Set<Point<int>> scaffolding = {};
  Point<int>? robot;
  Dir? dir;
  List<String> lines = mapStr.trim().split('\n');

  for (int y = 0; y < lines.length; y++) {
    String line = lines[y];
    for (int x = 0; x < line.length; x++) {
      int charCode = line.codeUnitAt(x);
      Point<int> p = Point(x, y);
      switch (String.fromCharCode(charCode)) {
        case '^':
        case '>':
        case 'v':
        case '<':
          robot = p;
          dir = DirExt.fromByte(charCode);
          scaffolding.add(p);
          break;
        case '#':
          scaffolding.add(p);
          break;
      }
    }
  }

  if (robot == null || dir == null) {
      throw Exception("Robot not found on map");
  }
  return (scaffolding, robot, dir);
}

String findPath(Set<Point<int>> scaffolding, Point<int> robot, Dir dir) {
  List<String> sections = [];
  Point<int> currentPos = robot;
  Dir currentDir = dir;
  int dist = 0;

  while (true) {
    Point<int> nextPos = currentPos + currentDir.point;
    if (scaffolding.contains(nextPos)) {
      currentPos = nextPos;
      dist++;
      continue;
    }

    if (dist > 0) {
      sections.add(dist.toString());
    }

    Dir nextDir = currentDir.next(); // Try right
    Point<int> rightPos = currentPos + nextDir.point;
    if (scaffolding.contains(rightPos)) {
      currentDir = nextDir;
      currentPos = rightPos;
      dist = 1;
      sections.add('R');
    } else {
      nextDir = currentDir.prev(); // Try left
      Point<int> leftPos = currentPos + nextDir.point;
      if (scaffolding.contains(leftPos)) {
        currentDir = nextDir;
        currentPos = leftPos;
        dist = 1;
        sections.add('L');
      } else {
        break; // No more moves
      }
    }
  }
  return sections.join(',');
}

(String, String, String, String)? encode(String path) {
  List<String> cmds = path.split(',');

   bool checkCompression(List<String> main, List<String> a, List<String> b, List<String> c) {
        if (main.join(',').length > 20 || a.join(',').length > 20 || b.join(',').length > 20 || c.join(',').length > 20) {
            return false;
        }

        List<String> currentPath = List.from(cmds);
        List<String> resultMain = [];

        while(currentPath.isNotEmpty) {
            if (currentPath.length >= a.length && currentPath.sublist(0, a.length).join(',') == a.join(',')) {
                resultMain.add('A');
                currentPath.removeRange(0, a.length);
            } else if (currentPath.length >= b.length && currentPath.sublist(0, b.length).join(',') == b.join(',')) {
                 resultMain.add('B');
                currentPath.removeRange(0, b.length);
            } else if (currentPath.length >= c.length && currentPath.sublist(0, c.length).join(',') == c.join(',')) {
                 resultMain.add('C');
                currentPath.removeRange(0, c.length);
            } else {
                return false; // Cannot compress further
            }
        }
        
        if (resultMain.join(',').length <= 20) {
             return true;
        }
        return false;
    }


  for (int lenA = 2; lenA <= 10; lenA += 2) { // Length in commands (L/R + number)
    List<String> a = cmds.sublist(0, lenA);
    if (a.join(',').length > 20) continue;

    for (int startB = lenA; startB < cmds.length; startB++) {
       if (cmds.length >= startB + a.length && cmds.sublist(startB, startB + a.length).join(',') == a.join(',')) {
           continue; // Skip if B starts with A
       }

      for (int lenB = 2; lenB <= 10; lenB += 2) {
         if (startB + lenB > cmds.length) continue;
        List<String> b = cmds.sublist(startB, startB + lenB);
         if (b.join(',').length > 20) continue;
        if (a.join(',') == b.join(',')) continue;


        for (int startC = startB + lenB; startC < cmds.length; startC++) {
           if ((cmds.length >= startC + a.length && cmds.sublist(startC, startC + a.length).join(',') == a.join(',')) ||
               (cmds.length >= startC + b.length && cmds.sublist(startC, startC + b.length).join(',') == b.join(','))) {
               continue; // Skip if C starts with A or B
            }

          for (int lenC = 2; lenC <= 10; lenC += 2) {
             if (startC + lenC > cmds.length) continue;
            List<String> c = cmds.sublist(startC, startC + lenC);
             if (c.join(',').length > 20) continue;
            if (a.join(',') == c.join(',') || b.join(',') == c.join(',')) continue;


            List<String> mainRoutine = [];
            List<String> tempPath = List.from(cmds);
            bool possible = true;

            while (tempPath.isNotEmpty) {
                if (tempPath.length >= a.length && tempPath.sublist(0, a.length).join(',') == a.join(',')) {
                     mainRoutine.add('A');
                    tempPath.removeRange(0, a.length);
                } else if (tempPath.length >= b.length && tempPath.sublist(0, b.length).join(',') == b.join(',')) {
                     mainRoutine.add('B');
                    tempPath.removeRange(0, b.length);
                } else if (tempPath.length >= c.length && tempPath.sublist(0, c.length).join(',') == c.join(',')) {
                    mainRoutine.add('C');
                    tempPath.removeRange(0, c.length);
                } else {
                    possible = false;
                    break;
                }
            }

            if (possible && mainRoutine.join(',').length <= 20) {
                return (mainRoutine.join(','), a.join(','), b.join(','), c.join(','));
            }
          }
        }
      }
    }
  }
  return null; // Should not happen for valid AoC input
}


int dust(List<int> program, Set<Point<int>> scaffolding, Point<int> robot, Dir dir) {
  String pathStr = findPath(scaffolding, robot, dir);
  var encoded = encode(pathStr);

  if (encoded == null) {
    throw Exception("Could not encode path");
  }

  String seq = encoded.$1;
  String a = encoded.$2;
  String b = encoded.$3;
  String c = encoded.$4;

  String inputString = "$seq\n$a\n$b\n$c\nn\n";
  List<int> inputAscii = inputString.codeUnits.toList();

  List<int> programCopy = List.from(program);
  programCopy[0] = 2; // Activate the robot

  Machine machine = Machine(programCopy, inputAscii);
  List<int> output = machine.run();

  return output.last; // The final output is the amount of dust
}


void main() {
  String content = File('input.txt').readAsStringSync().trim();
  List<int> program = content.split(',').map(int.parse).toList();

  var (scaffolding, robot, dir) = parse(program);
  int result = dust(program, scaffolding, robot, dir);

  print(result);
}
