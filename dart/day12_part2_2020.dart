
import 'dart:io';

void main() {
  List<String> instructions = File('input.txt').readAsLinesSync();
  
  int x = 0;
  int y = 0;
  int wx = 10;
  int wy = 1;
  
  for (String instruction in instructions) {
    String action = instruction.substring(0, 1);
    int value = int.parse(instruction.substring(1));
    
    if (action == 'N') {
      wy += value;
    } else if (action == 'S') {
      wy -= value;
    } else if (action == 'E') {
      wx += value;
    } else if (action == 'W') {
      wx -= value;
    } else if (action == 'L' || action == 'R') {
      int times = value ~/ 90;
      for (int i = 0; i < times; i++) {
        int temp = 0;
        if (action == 'L') {
          temp = wx;
          wx = -wy;
          wy = temp;
        } else {
          temp = wx;
          wx = wy;
          wy = -temp;
        }
      }
    } else if (action == 'F') {
      x += wx * value;
      y += wy * value;
    }
  }
  
  print((x.abs() + y.abs()));
}
