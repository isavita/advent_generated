
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<List<int>> coordinates = lines.map((line) => line.split(', ').map(int.parse).toList()).toList();

  int maxX = coordinates.map((coord) => coord[0]).reduce((value, element) => value > element ? value : element);
  int maxY = coordinates.map((coord) => coord[1]).reduce((value, element) => value > element ? value : element);

  List<int> areas = List.filled(coordinates.length, 0);
  List<int> region = List.filled(maxY + 1, 0);

  for (int y = 0; y <= maxY; y++) {
    for (int x = 0; x <= maxX; x++) {
      int totalDistance = 0;
      for (int i = 0; i < coordinates.length; i++) {
        totalDistance += (x - coordinates[i][0]).abs() + (y - coordinates[i][1]).abs();
      }
      if (totalDistance < 10000) {
        region[y]++;
      }

      int minDistance = coordinates.map((coord) => (x - coord[0]).abs() + (y - coord[1]).abs()).reduce((value, element) => value < element ? value : element);
      int closest = coordinates.indexWhere((coord) => (x - coord[0]).abs() + (y - coord[1]).abs() == minDistance);
      if (x == 0 || x == maxX || y == 0 || y == maxY) {
        areas[closest] = -1;
      } else if (areas[closest] != -1) {
        areas[closest]++;
      }
    }
  }

  int largestArea = areas.where((area) => area != -1).reduce((value, element) => value > element ? value : element);
  int regionSize = region.reduce((value, element) => value + element);

  print(largestArea);
  print(regionSize);
}
