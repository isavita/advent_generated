
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<List<int>> fabric = List.generate(1000, (_) => List.filled(1000, 0));
  List<int> nonOverlappingClaims = [];

  for (String line in lines) {
    List<String> parts = line.split(' ');
    List<int> coordinates = parts[2].replaceAll(':', '').split(',').map(int.parse).toList();
    List<int> dimensions = parts[3].split('x').map(int.parse).toList();

    bool overlaps = false;
    for (int i = coordinates[0]; i < coordinates[0] + dimensions[0]; i++) {
      for (int j = coordinates[1]; j < coordinates[1] + dimensions[1]; j++) {
        if (fabric[i][j] == 1) {
          overlaps = true;
          break;
        }
      }
      if (overlaps) break;
    }

    if (!overlaps) {
      nonOverlappingClaims.add(int.parse(parts[0].substring(1)));
    }

    for (int i = coordinates[0]; i < coordinates[0] + dimensions[0]; i++) {
      for (int j = coordinates[1]; j < coordinates[1] + dimensions[1]; j++) {
        fabric[i][j]++;
      }
    }
  }

  int count = 0;
  for (int i = 0; i < fabric.length; i++) {
    for (int j = 0; j < fabric[i].length; j++) {
      if (fabric[i][j] >= 2) {
        count++;
      }
    }
  }

  print(count);
  print(nonOverlappingClaims);
}
