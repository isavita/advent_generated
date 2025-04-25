
import 'dart:io';
import 'dart:math';
import 'dart:collection';

typedef Point = ({int x, int y});

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
        orElse: () => throw Exception('Unknown opcode: $val'));
  }
}

class Instruction {
  final Opcode opcode;
  final List<Mode> modes;

  Instruction(int instruction)
      : opcode = Opcode.fromInt(instruction % 100),
        modes = [
          Mode.values[(instruction ~/ 100) % 10],
          Mode.values[(instruction ~/ 1000) % 10],
          Mode.values[(instruction ~/ 10000) % 10],
        ];
}

class IntcodeComputer {
  final Map<int, int> _data;
  int _ip = 0;
  int _relativeBase = 0;
  final Queue<int> _inputQueue = Queue<int>();
  final Queue<int> _outputQueue = Queue<int>();
  bool _halted = false;
  bool _waitingForInput = false;

  IntcodeComputer(List<int> program) : _data = {} {
    for (int i = 0; i < program.length; i++) {
      _data[i] = program[i];
    }
  }

  int _get(int index, Mode mode) {
    int address;
    switch (mode) {
      case Mode.position:
        address = _data[_ip + index] ?? 0;
        break;
      case Mode.immediate:
        return _data[_ip + index] ?? 0;
      case Mode.relative:
        address = (_data[_ip + index] ?? 0) + _relativeBase;
        break;
    }
    return _data[address] ?? 0;
  }

  void _set(int index, Mode mode, int value) {
    int address;
    switch (mode) {
      case Mode.position:
        address = _data[_ip + index] ?? 0;
        break;
      case Mode.relative:
        address = (_data[_ip + index] ?? 0) + _relativeBase;
        break;
      case Mode.immediate:
        throw Exception("Cannot set in immediate mode");
    }
    _data[address] = value;
  }

  void addInput(int value) {
    _inputQueue.addLast(value);
    _waitingForInput = false;
  }

  int? getOutput() {
    return _outputQueue.isNotEmpty ? _outputQueue.removeFirst() : null;
  }

  bool get isHalted => _halted;
  bool get isWaitingForInput => _waitingForInput;

  void run() {
    while (!_halted && !_waitingForInput) {
      _step();
    }
  }

  void _step() {
    if (_halted) return;

    final instruction = Instruction(_data[_ip] ?? 0);
    final modes = instruction.modes;

    switch (instruction.opcode) {
      case Opcode.add:
        final val = _get(1, modes[0]) + _get(2, modes[1]);
        _set(3, modes[2], val);
        _ip += 4;
        break;
      case Opcode.mul:
        final val = _get(1, modes[0]) * _get(2, modes[1]);
        _set(3, modes[2], val);
        _ip += 4;
        break;
      case Opcode.input:
        if (_inputQueue.isEmpty) {
          _waitingForInput = true;
          return;
        }
        _set(1, modes[0], _inputQueue.removeFirst());
        _ip += 2;
        break;
      case Opcode.output:
        _outputQueue.addLast(_get(1, modes[0]));
        _ip += 2;
        break;
      case Opcode.jt:
        if (_get(1, modes[0]) != 0) {
          _ip = _get(2, modes[1]);
        } else {
          _ip += 3;
        }
        break;
      case Opcode.jf:
        if (_get(1, modes[0]) == 0) {
          _ip = _get(2, modes[1]);
        } else {
          _ip += 3;
        }
        break;
      case Opcode.lt:
        _set(3, modes[2], _get(1, modes[0]) < _get(2, modes[1]) ? 1 : 0);
        _ip += 4;
        break;
      case Opcode.eq:
        _set(3, modes[2], _get(1, modes[0]) == _get(2, modes[1]) ? 1 : 0);
        _ip += 4;
        break;
      case Opcode.rbo:
        _relativeBase += _get(1, modes[0]);
        _ip += 2;
        break;
      case Opcode.halt:
        _halted = true;
        break;
    }
  }
}

enum Direction {
  north(1, (x: 0, y: 1)),
  south(2, (x: 0, y: -1)),
  west(3, (x: -1, y: 0)),
  east(4, (x: 1, y: 0));

  final int command;
  final Point offset;
  const Direction(this.command, this.offset);

  Direction get reverse {
    switch (this) {
      case Direction.north: return Direction.south;
      case Direction.south: return Direction.north;
      case Direction.west: return Direction.east;
      case Direction.east: return Direction.west;
    }
  }
}

class Pathfinder {
  final IntcodeComputer computer;
  final Map<Point, int> grid = {}; // 0: Wall, 1: Empty, 2: Oxygen
  Point currentPos = (x: 0, y: 0);
  Point? oxygenPos;
  final Set<Point> visitedDuringExplore = {};

  Pathfinder(List<int> program) : computer = IntcodeComputer(program) {
    grid[currentPos] = 1;
  }

  void explore() {
    _exploreRecursive(currentPos);
  }

  void _exploreRecursive(Point pos) {
    visitedDuringExplore.add(pos);

    for (final dir in Direction.values) {
      final nextPos = (x: pos.x + dir.offset.x, y: pos.y + dir.offset.y);

      if (!grid.containsKey(nextPos)) {
        computer.addInput(dir.command);
        computer.run();
        final status = computer.getOutput();
        if (status == null) throw Exception("No output from computer");

        grid[nextPos] = status;

        if (status != 0) { // Not a wall
          currentPos = nextPos;
          if (status == 2) {
            oxygenPos = currentPos;
          }
          _exploreRecursive(currentPos);

          // Backtrack
          final reverseDir = dir.reverse;
          computer.addInput(reverseDir.command);
          computer.run();
          final backtrackStatus = computer.getOutput();
          if (backtrackStatus == 0 || backtrackStatus == null) {
             throw Exception("Failed to backtrack");
          }
          currentPos = (x: currentPos.x + reverseDir.offset.x, y: currentPos.y + reverseDir.offset.y);
          assert(currentPos == pos);
        }
      }
    }
  }

  int findLongestPathFromOxygen() {
    if (oxygenPos == null) return -1;

    final queue = Queue<MapEntry<Point, int>>();
    final visited = <Point>{};
    int maxDist = 0;

    queue.addLast(MapEntry(oxygenPos!, 0));
    visited.add(oxygenPos!);

    while (queue.isNotEmpty) {
      final entry = queue.removeFirst();
      final pos = entry.key;
      final dist = entry.value;

      maxDist = max(maxDist, dist);

      for (final dir in Direction.values) {
        final nextPos = (x: pos.x + dir.offset.x, y: pos.y + dir.offset.y);
        if (grid.containsKey(nextPos) && grid[nextPos] != 0 && !visited.contains(nextPos)) {
          visited.add(nextPos);
          queue.addLast(MapEntry(nextPos, dist + 1));
        }
      }
    }
    return maxDist;
  }
}

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  final program = input.split(',').map(int.parse).toList();

  final pathfinder = Pathfinder(program);
  pathfinder.explore();
  final result = pathfinder.findLongestPathFromOxygen();
  print(result);
}
