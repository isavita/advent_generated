
import 'dart:io';
import 'dart:collection';

class VM {
  Map<int, int> code;
  int ip;
  Queue<int> input;
  Queue<int> output;
  int relativeBase;

  VM(String filename)
      : code = {},
        ip = 0,
        input = Queue<int>(),
        output = Queue<int>(),
        relativeBase = 0 {
    load(filename);
  }

  void load(String filename) {
    final file = File(filename);
    final lines = file.readAsStringSync().trim().split(',');
    code = {};
    for (int i = 0; i < lines.length; i++) {
      code[i] = int.parse(lines[i]);
    }
    ip = 0;
    relativeBase = 0;
  }

  void run() {
    int arity;

    while (true) {
      final cmd = Cmd(code[ip] ?? 0);

      switch (cmd.opCode()) {
        case 1:
          arity = 3;
          final params = getParamsAddresses(ip, cmd, arity);
          code[params[2]] = (code[params[0]] ?? 0) + (code[params[1]] ?? 0);
          break;

        case 2:
          arity = 3;
          final params = getParamsAddresses(ip, cmd, arity);
          code[params[2]] = (code[params[0]] ?? 0) * (code[params[1]] ?? 0);
          break;

        case 3:
          arity = 1;
          final params = getParamsAddresses(ip, cmd, arity);
          code[params[0]] = input.removeFirst();
          break;

        case 4:
          arity = 1;
          final params = getParamsAddresses(ip, cmd, arity);
          output.add(code[params[0]] ?? 0);
          break;

        case 5:
          arity = 2;
          final params = getParamsAddresses(ip, cmd, arity);
          if ((code[params[0]] ?? 0) != 0) {
            ip = code[params[1]] ?? 0;
            continue;
          }
          break;

        case 6:
          arity = 2;
          final params = getParamsAddresses(ip, cmd, arity);
          if ((code[params[0]] ?? 0) == 0) {
            ip = code[params[1]] ?? 0;
            continue;
          }
          break;

        case 7:
          arity = 3;
          final params = getParamsAddresses(ip, cmd, arity);
          code[params[2]] =
              (code[params[0]] ?? 0) < (code[params[1]] ?? 0) ? 1 : 0;
          break;

        case 8:
          arity = 3;
          final params = getParamsAddresses(ip, cmd, arity);
          code[params[2]] =
              (code[params[0]] ?? 0) == (code[params[1]] ?? 0) ? 1 : 0;
          break;

        case 9:
          arity = 1;
          final params = getParamsAddresses(ip, cmd, arity);
          relativeBase += code[params[0]] ?? 0;
          break;

        case 99:
          return;

        default:
          throw Exception('Not an opcode $cmd');
      }

      ip += arity + 1;
    }
  }

  List<int> getParamsAddresses(int pos, Cmd cmd, int arity) {
    final modes = cmd.modes(arity);
    final results = List<int>.filled(arity, 0);
    for (int i = 0; i < arity; i++) {
      results[i] = getParamAddress(pos + i + 1, modes[i]);
    }
    return results;
  }

  int getParamAddress(int pos, Mode mode) {
    switch (mode) {
      case Mode.position:
        return code[pos] ?? 0;
      case Mode.immediate:
        return pos;
      case Mode.relative:
        return relativeBase + (code[pos] ?? 0);
    }
  }
}

enum Mode { position, immediate, relative }

class Cmd {
  final int value;

  Cmd(this.value);

  int opCode() {
    return value % 100;
  }

  List<Mode> modes(int arity) {
    final modeSection = value ~/ 100;
    final modes = List<Mode>.filled(arity, Mode.position);
    for (int i = 0; i < arity; i++) {
      final modeValue = (modeSection ~/ (10.pow(i))) % 10;
      modes[i] = Mode.values[modeValue];
    }
    return modes;
  }
}

void sendString(Queue<int> input, String s) {
  for (final r in s.runes) {
    input.add(r);
  }
  input.add(10);
}

void reader(Queue<int> output) {
  for (final c in output) {
    if (c > 127) {
      print(c);
      return;
    }
  }
}

void main() {
  final vm = VM('input.txt');

  final instructions = [
    'NOT A J',
    'NOT B T',
    'OR T J',
    'NOT C T',
    'OR T J',
    'AND D J',
    'NOT A T',
    'AND A T',
    'OR E T',
    'OR H T',
    'AND T J',
    'RUN',
  ];

  for (final i in instructions) {
    sendString(vm.input, i);
  }

  vm.run();
  reader(vm.output);
}

extension Pow on num {
  num pow(num exponent) {
    if (exponent == 0) return 1;
    if (exponent == 1) return this;
    if (exponent < 0) return 1 / pow(-exponent);
    final half = pow(exponent ~/ 2);
    return half * half * (exponent % 2 == 0 ? 1 : this);
  }
}
