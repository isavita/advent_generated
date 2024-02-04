
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<List<int>> coordinates = lines.map((line) => line.split(', ').map(int.parse).toList()).toList();

  int maxX = coordinates.map((coord) => coord[0]).reduce((value, element) => value > element ? value : element);
  int maxY = coordinates.map((coord) => coord[1]).reduce((value, element) => value > element ? value : element);

  List<int> areas = List.filled(coordinates.length, 0);
  List<bool> infinite = List.filled(coordinates.length, false);

  for (int x = 0; x <= maxX; x++) {
    for (int y = 0; y <= maxY; y++) {
      int minDistance = maxX + maxY;
      int closest = -1;
      bool equidistant = false;

      for (int i = 0; i < coordinates.length; i++) {
        int distance = (x - coordinates[i][0]).abs() + (y - coordinates[i][1]).abs();

        if (distance < minDistance) {
          minDistance = distance;
          closest = i;
          equidistant = false;
        } else if (distance == minDistance) {
          equidistant = true;
        }
      }

      if (!equidistant) {
        if (x == 0 || x == maxX || y == 0 || y == maxY) {
          infinite[closest] = true;
        } else {
          areas[closest]++;
        }
      }
    }
  }

  int largestArea = 0;
  for (int i = 0; i < coordinates.length; i++) {
    if (!infinite[i] && areas[i] > largestArea) {
      largestArea = areas[i];
    }
  }

  print(largestArea);
}
