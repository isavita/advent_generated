
import 'dart:io';

void main() {
  List<String> instructions = File('input.txt').readAsLinesSync();

  int x = 0;
  int y = 0;
  int direction = 0;

  for (String instruction in instructions) {
    String action = instruction.substring(0, 1);
    int value = int.parse(instruction.substring(1));

    if (action == 'N') {
      y += value;
    } else if (action == 'S') {
      y -= value;
    } else if (action == 'E') {
      x += value;
    } else if (action == 'W') {
      x -= value;
    } else if (action == 'L') {
      direction = (direction - value + 360) % 360;
    } else if (action == 'R') {
      direction = (direction + value) % 360;
    } else if (action == 'F') {
      if (direction == 0) {
        x += value;
      } else if (direction == 90) {
        y -= value;
      } else if (direction == 180) {
        x -= value;
      } else if (direction == 270) {
        y += value;
      }
    }
  }

  print((x.abs() + y.abs()));
}
