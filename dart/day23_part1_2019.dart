
import 'dart:io';

class IntcodeComputer {
  List<int> memory;
  int ip = 0;
  int relativeBase = 0;
  List<int> inputQueue = [];
  List<int> outputQueue = [];
  bool halted = false;

  IntcodeComputer(this.memory);

  void run() {
    while (!halted) {
      int opcode = memory[ip] % 100;
      int mode1 = (memory[ip] ~/ 100) % 10;
      int mode2 = (memory[ip] ~/ 1000) % 10;
      int mode3 = (memory[ip] ~/ 10000) % 10;

      switch (opcode) {
        case 1:
          setParameter(3, mode3, getParameter(1, mode1) + getParameter(2, mode2));
          ip += 4;
          break;
        case 2:
          setParameter(3, mode3, getParameter(1, mode1) * getParameter(2, mode2));
          ip += 4;
          break;
        case 3:
          if (inputQueue.isEmpty) {
            return;
          }
          setParameter(1, mode1, inputQueue.removeAt(0));
          ip += 2;
          break;
        case 4:
          outputQueue.add(getParameter(1, mode1));
          ip += 2;
          break;
        case 5:
          if (getParameter(1, mode1) != 0) {
            ip = getParameter(2, mode2);
          } else {
            ip += 3;
          }
          break;
        case 6:
          if (getParameter(1, mode1) == 0) {
            ip = getParameter(2, mode2);
          } else {
            ip += 3;
          }
          break;
        case 7:
          setParameter(
              3,
              mode3,
              getParameter(1, mode1) < getParameter(2, mode2)
                  ? 1
                  : 0);
          ip += 4;
          break;
        case 8:
          setParameter(
              3,
              mode3,
              getParameter(1, mode1) == getParameter(2, mode2)
                  ? 1
                  : 0);
          ip += 4;
          break;
        case 9:
          relativeBase += getParameter(1, mode1);
          ip += 2;
          break;
        case 99:
          halted = true;
          break;
        default:
          throw Exception('Unknown opcode: $opcode');
      }
    }
  }

  int getParameter(int offset, int mode) {
    int address;
    switch (mode) {
      case 0:
        address = memory[ip + offset];
        break;
      case 1:
        address = ip + offset;
        break;
      case 2:
        address = memory[ip + offset] + relativeBase;
        break;
      default:
        throw Exception('Unknown parameter mode: $mode');
    }
    if (address >= memory.length) {
      memory.addAll(List.filled(address - memory.length + 1, 0));
    }
    return memory[address];
  }

  void setParameter(int offset, int mode, int value) {
    int address;
    switch (mode) {
      case 0:
        address = memory[ip + offset];
        break;
      case 1:
        address = ip + offset;
        break;
      case 2:
        address = memory[ip + offset] + relativeBase;
        break;
      default:
        throw Exception('Unknown parameter mode: $mode');
    }
    if (address >= memory.length) {
      memory.addAll(List.filled(address - memory.length + 1, 0));
    }
    memory[address] = value;
  }
}

void main() {
  String input = File('input.txt').readAsStringSync();
  List<int> program = input.split(',').map(int.parse).toList();

  List<IntcodeComputer> computers = List.generate(50, (i) {
    IntcodeComputer computer = IntcodeComputer(List.from(program));
    computer.inputQueue.add(i);
    return computer;
  });

  Map<int, List<List<int>>> packetQueues = {};

  while (true) {
    for (int i = 0; i < 50; i++) {
      IntcodeComputer computer = computers[i];
      if (computer.inputQueue.isEmpty) {
        computer.inputQueue.add(-1);
      }
      computer.run();
      while (computer.outputQueue.length >= 3) {
        int address = computer.outputQueue.removeAt(0);
        int x = computer.outputQueue.removeAt(0);
        int y = computer.outputQueue.removeAt(0);

        if (address == 255) {
          print(y);
          return;
        }

        if (!packetQueues.containsKey(address)) {
          packetQueues[address] = [];
        }
        packetQueues[address]!.add([x, y]);
      }
      if (packetQueues.containsKey(i)) {
        for (List<int> packet in packetQueues[i]!) {
          computer.inputQueue.addAll(packet);
        }
        packetQueues.remove(i);
      }
    }
  }
}
