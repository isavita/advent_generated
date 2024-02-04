
import 'dart:io';

void main() {
  List<String> instructions = File('input.txt').readAsLinesSync();
  
  List<List<int>> keypad = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ];
  
  int x = 1;
  int y = 1;
  
  String code = '';
  
  for (String instruction in instructions) {
    for (int i = 0; i < instruction.length; i++) {
      if (instruction[i] == 'U' && y > 0) y--;
      else if (instruction[i] == 'D' && y < 2) y++;
      else if (instruction[i] == 'L' && x > 0) x--;
      else if (instruction[i] == 'R' && x < 2) x++;
    }
    
    code += keypad[y][x].toString();
  }
  
  print(code);
}
