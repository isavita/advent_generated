
import 'dart:io';

void main() {
  List<String> instructions = File('input.txt').readAsLinesSync();
  
  List<List<String>> keypad = [
    ['0', '0', '1', '0', '0'],
    ['0', '2', '3', '4', '0'],
    ['5', '6', '7', '8', '9'],
    ['0', 'A', 'B', 'C', '0'],
    ['0', '0', 'D', '0', '0']
  ];
  
  int x = 0;
  int y = 2;
  
  List<String> code = [];
  
  for (String instruction in instructions) {
    for (int i = 0; i < instruction.length; i++) {
      switch (instruction[i]) {
        case 'U':
          if (y > 0 && keypad[y - 1][x] != '0') y--;
          break;
        case 'D':
          if (y < 4 && keypad[y + 1][x] != '0') y++;
          break;
        case 'L':
          if (x > 0 && keypad[y][x - 1] != '0') x--;
          break;
        case 'R':
          if (x < 4 && keypad[y][x + 1] != '0') x++;
          break;
      }
    }
    
    code.add(keypad[y][x]);
  }
  
  print(code.join());
}
