
import 'dart:io';

void main() {
  List<int> memory = File('input.txt').readAsStringSync().trim().split(',').map(int.parse).toList();

  int pointer = 0;

  while (memory[pointer] != 99) {
    String instruction = memory[pointer].toString().padLeft(5, '0');
    int opcode = int.parse(instruction.substring(3));

    if (opcode == 1) {
      int param1 = memory[pointer + 1];
      int param2 = memory[pointer + 2];
      int param3 = memory[pointer + 3];

      int val1 = instruction[2] == '0' ? memory[param1] : param1;
      int val2 = instruction[1] == '0' ? memory[param2] : param2;

      memory[param3] = val1 + val2;

      pointer += 4;
    } else if (opcode == 2) {
      int param1 = memory[pointer + 1];
      int param2 = memory[pointer + 2];
      int param3 = memory[pointer + 3];

      int val1 = instruction[2] == '0' ? memory[param1] : param1;
      int val2 = instruction[1] == '0' ? memory[param2] : param2;

      memory[param3] = val1 * val2;

      pointer += 4;
    } else if (opcode == 3) {
      int param1 = memory[pointer + 1];

      memory[param1] = 1; // Input value

      pointer += 2;
    } else if (opcode == 4) {
      int param1 = memory[pointer + 1];

      print(memory[param1]); // Output value

      pointer += 2;
    } else {
      print('Unknown opcode: $opcode');
      break;
    }
  }
}
