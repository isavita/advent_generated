import 'dart:io';

void main() {
  List<String> directions = File('input.txt').readAsStringSync().split(', ');

  int x = 0;
  int y = 0;
  int direction = 0;

  for (String step in directions) {
    String turn = step.substring(0, 1);
    int distance = int.parse(step.substring(1));

    if (turn == 'R') {
      direction = (direction + 1) % 4;
    } else {
      direction = (direction + 3) % 4;
    }

    if (direction == 0) {
      y += distance;
    } else if (direction == 1) {
      x += distance;
    } else if (direction == 2) {
      y -= distance;
    } else {
      x -= distance;
    }
  }

  print((x.abs() + y.abs()));
}