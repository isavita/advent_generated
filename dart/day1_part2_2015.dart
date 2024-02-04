
import 'dart:io';

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  String input = lines[0];

  int floor = 0;
  int position = 0;

  for (int i = 0; i < input.length; i++) {
    if (input[i] == '(') {
      floor++;
    } else {
      floor--;
    }

    if (floor == -1) {
      position = i + 1;
      break;
    }
  }

  print('Part One: $floor');
  print('Part Two: $position');
}
