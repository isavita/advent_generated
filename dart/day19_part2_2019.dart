
import 'dart:io';

class VM {
  late Map<int, int> code;
  int ip = 0;
  int relativeBase = 0;
  final List<int> input = [];
  final List<int> output = [];
  int inputIndex = 0;

  VM(String filename) {
    load(filename);
  }

  void load(String filename) {
    final lines = File(filename).readAsStringSync().trim().split(',');
    code = {for (int i = 0; i < lines.length; i++) i: int.parse(lines[i])};
  }

  void run() {
    int arity;

    while (true) {
      final cmd = Cmd(code[ip]!);

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
          code[params[0]] = input[inputIndex++];
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
            ip = (code[params[1]] ?? 0);
            continue;
          }
          break;

        case 6:
          arity = 2;
          final params = getParamsAddresses(ip, cmd, arity);
          if ((code[params[0]] ?? 0) == 0) {
            ip = (code[params[1]] ?? 0);
            continue;
          }
          break;

        case 7:
          arity = 3;
          final params = getParamsAddresses(ip, cmd, arity);
          code[params[2]] = (code[params[0]] ?? 0) < (code[params[1]] ?? 0) ? 1 : 0;
          break;

        case 8:
          arity = 3;
          final params = getParamsAddresses(ip, cmd, arity);
          code[params[2]] = (code[params[0]] ?? 0) == (code[params[1]] ?? 0) ? 1 : 0;
          break;

        case 9:
          arity = 1;
          final params = getParamsAddresses(ip, cmd, arity);
          relativeBase += (code[params[0]] ?? 0);
          break;

        case 99:
          return;

        default:
          throw Exception('Invalid opcode ${cmd.opCode()}');
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
      default:
        throw Exception('Invalid mode');
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
      final modeValue = (modeSection ~/ (1 * _pow(10, i))) % 10;
      modes[i] = Mode.values[modeValue];
    }
    return modes;
  }
    int _pow(int base, int exponent) {
    int result = 1;
    for (int i = 0; i < exponent; i++) {
      result *= base;
    }
    return result;
  }
}

bool beam(int x, int y) {
  final vm = VM('input.txt');
  vm.input.addAll([x, y]);
  vm.run();
  return vm.output.last == 1;
}

void main() {
  int y = 20;
  int x = 0;
  int size = 99;

  while (true) {
    if (!beam(x, y)) {
      x++;
      continue;
    }
    if (!beam(x + size, y)) {
      y++;
      continue;
    }
    if (!beam(x, y + size)) {
      x++;
      continue;
    }
    print(x * 10000 + y);
    return;
  }
}
