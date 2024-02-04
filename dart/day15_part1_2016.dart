
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<List<int>> discs = [];

  for (String line in lines) {
    List<String> parts = line.split(' ');
    int positions = int.parse(parts[3]);
    int initial = int.parse(parts[parts.length - 1].replaceAll('.', ''));
    discs.add([positions, initial]);
  }

  int time = 0;
  while (true) {
    bool success = true;
    for (int i = 0; i < discs.length; i++) {
      if ((discs[i][1] + time + i + 1) % discs[i][0] != 0) {
        success = false;
        break;
      }
    }
    if (success) {
      print(time);
      break;
    }
    time++;
  }
}
