
import 'dart:io';

void main() {
  List<List<int>> lights = List.generate(1000, (_) => List.filled(1000, 0));

  File file = File('input.txt');
  List<String> instructions = file.readAsLinesSync();

  for (String instruction in instructions) {
    List<String> parts = instruction.split(' ');
    if (parts[0] == 'turn') {
      List<int> start = parts[2].split(',').map(int.parse).toList();
      List<int> end = parts[4].split(',').map(int.parse).toList();
      if (parts[1] == 'on') {
        for (int i = start[0]; i <= end[0]; i++) {
          for (int j = start[1]; j <= end[1]; j++) {
            lights[i][j] = 1;
          }
        }
      } else {
        for (int i = start[0]; i <= end[0]; i++) {
          for (int j = start[1]; j <= end[1]; j++) {
            lights[i][j] = 0;
          }
        }
      }
    } else {
      List<int> start = parts[1].split(',').map(int.parse).toList();
      List<int> end = parts[3].split(',').map(int.parse).toList();
      for (int i = start[0]; i <= end[0]; i++) {
        for (int j = start[1]; j <= end[1]; j++) {
          lights[i][j] = 1 - lights[i][j];
        }
      }
    }
  }

  int count = 0;
  for (int i = 0; i < 1000; i++) {
    for (int j = 0; j < 1000; j++) {
      count += lights[i][j];
    }
  }

  print(count);
}
