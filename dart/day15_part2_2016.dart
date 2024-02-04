
import 'dart:io';

void main() {
  List<List<int>> discs = [];
  List<int> positions = [];
  List<int> initialPositions = [];
  List<int> initialDiscs = [];
  int time = 0;

  File file = new File('input.txt');
  List<String> lines = file.readAsLinesSync();

  for (String line in lines) {
    List<String> parts = line.split(' ');
    int discNumber = int.parse(parts[1].substring(1));
    int positionsCount = int.parse(parts[3]);
    int initialPosition = int.parse(parts[parts.length - 1].substring(0, parts[parts.length - 1].length - 1));

    initialDiscs.add(discNumber);
    initialPositions.add(positionsCount);
    positions.add(positionsCount);
    discs.add([discNumber, positionsCount, initialPosition]);
  }

  while (true) {
    bool success = true;
    for (int i = 0; i < discs.length; i++) {
      if ((discs[i][2] + time + i + 1) % discs[i][1] != 0) {
        success = false;
        break;
      }
    }

    if (success) {
      break;
    }

    time++;
  }

  print(time);

  discs.add([initialDiscs.length + 1, 11, 0]);
  positions.add(11);
  initialDiscs.add(initialDiscs.length + 1);
  initialPositions.add(11);

  time = 0;

  while (true) {
    bool success = true;
    for (int i = 0; i < discs.length; i++) {
      if ((discs[i][2] + time + i + 1) % discs[i][1] != 0) {
        success = false;
        break;
      }
    }

    if (success) {
      break;
    }

    time++;
  }

  print(time);
}
