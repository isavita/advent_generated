
import 'dart:io';

class IntcodeComputer {
  List<int> memory;
  int ip = 0;
  int relativeBase = 0;
  List<int> inputQueue = [];
  List<int> outputQueue = [];

  IntcodeComputer(List<int> program) : memory = List.from(program);

  int getParameter(int mode, int offset) {
    int address;
    switch (mode) {
      case 0: // Position mode
        address = memory[ip + offset];
        break;
      case 1: // Immediate mode
        address = ip + offset;
        break;
      case 2: // Relative mode
        address = memory[ip + offset] + relativeBase;
        break;
      default:
        throw Exception('Invalid parameter mode: $mode');
    }
    if (address >= memory.length) {
      memory.addAll(List.filled(address - memory.length + 1, 0));
    }
    return memory[address];
  }

  void setParameter(int mode, int offset, int value) {
    int address;
    switch (mode) {
      case 0: // Position mode
        address = memory[ip + offset];
        break;
      case 2: // Relative mode
        address = memory[ip + offset] + relativeBase;
        break;
      default:
        throw Exception('Invalid parameter mode for set: $mode');
    }
    if (address >= memory.length) {
      memory.addAll(List.filled(address - memory.length + 1, 0));
    }
    memory[address] = value;
  }

  void run() {
    while (true) {
      int opcode = memory[ip] % 100;
      int mode1 = (memory[ip] ~/ 100) % 10;
      int mode2 = (memory[ip] ~/ 1000) % 10;
      int mode3 = (memory[ip] ~/ 10000) % 10;

      switch (opcode) {
        case 1: // Add
          setParameter(mode3, 3,
              getParameter(mode1, 1) + getParameter(mode2, 2));
          ip += 4;
          break;
        case 2: // Multiply
          setParameter(mode3, 3,
              getParameter(mode1, 1) * getParameter(mode2, 2));
          ip += 4;
          break;
        case 3: // Input
          if (inputQueue.isEmpty) return;
          setParameter(mode1, 1, inputQueue.removeAt(0));
          ip += 2;
          break;
        case 4: // Output
          outputQueue.add(getParameter(mode1, 1));
          ip += 2;
          break;
        case 5: // Jump-if-true
          if (getParameter(mode1, 1) != 0) {
            ip = getParameter(mode2, 2);
          } else {
            ip += 3;
          }
          break;
        case 6: // Jump-if-false
          if (getParameter(mode1, 1) == 0) {
            ip = getParameter(mode2, 2);
          } else {
            ip += 3;
          }
          break;
        case 7: // Less than
          setParameter(mode3, 3,
              getParameter(mode1, 1) < getParameter(mode2, 2) ? 1 : 0);
          ip += 4;
          break;
        case 8: // Equals
          setParameter(mode3, 3,
              getParameter(mode1, 1) == getParameter(mode2, 2) ? 1 : 0);
          ip += 4;
          break;
        case 9: // Adjust relative base
          relativeBase += getParameter(mode1, 1);
          ip += 2;
          break;
        case 99: // Halt
          return;
        default:
          throw Exception('Invalid opcode: $opcode');
      }
    }
  }
}

void main() {
  String input = File('input.txt').readAsStringSync().trim();
  List<int> program = input.split(',').map(int.parse).toList();

  List<IntcodeComputer> computers = List.generate(
      50, (i) => IntcodeComputer(program)..inputQueue.add(i));
  Map<int, List<List<int>>> packetQueues = {};
  List<int> natPacket = [];
  int? firstYTo255;
  int? lastYDelivered = null;
  Set<int> deliveredYValues = {};
  
  bool part1Solved = false;

  while (true) {
    bool idle = true;
    for (int i = 0; i < computers.length; i++) {
      computers[i].run();

      if (computers[i].outputQueue.isNotEmpty) {
        idle = false;
        for (int j = 0; j < computers[i].outputQueue.length; j += 3) {
          int address = computers[i].outputQueue[j];
          int x = computers[i].outputQueue[j + 1];
          int y = computers[i].outputQueue[j + 2];

          if (address == 255) {
            if (!part1Solved) {
              firstYTo255 = y;
              print("Part 1: $firstYTo255");
              part1Solved = true;
            }
            
            natPacket = [x, y];
          } else {
            packetQueues.putIfAbsent(address, () => []).add([x, y]);
          }
        }
        computers[i].outputQueue.clear();
      }

      if (packetQueues.containsKey(i) && packetQueues[i]!.isNotEmpty) {
        idle = false;
        List<int> packet = packetQueues[i]!.removeAt(0);
        computers[i].inputQueue.addAll(packet);
      } else {
        computers[i].inputQueue.add(-1);
      }
    }

    if (idle && natPacket.isNotEmpty) {
      if (deliveredYValues.contains(natPacket[1])) {
        lastYDelivered = natPacket[1];
        print("Part 2: $lastYDelivered");
        break;
      }
      deliveredYValues.add(natPacket[1]);
      packetQueues.putIfAbsent(0, () => []).add(natPacket);
    }
  }
}
