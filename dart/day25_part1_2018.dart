
import 'dart:io';

List<List<int>> parseInput(String filename) {
  List<List<int>> points = [];
  File file = File(filename);
  List<String> lines = file.readAsLinesSync();

  for (String line in lines) {
    List<int> coordinates = line.split(',').map(int.parse).toList();
    points.add(coordinates);
  }

  return points;
}

int manhattanDistance(List<int> p1, List<int> p2) {
  int distance = 0;
  for (int i = 0; i < p1.length; i++) {
    distance += (p1[i] - p2[i]).abs();
  }
  return distance;
}

void mergeConstellations(List<List<int>> points, int index, Set<int> visited) {
  visited.add(index);
  for (int i = 0; i < points.length; i++) {
    if (!visited.contains(i) && manhattanDistance(points[index], points[i]) <= 3) {
      mergeConstellations(points, i, visited);
    }
  }
}

void main() {
  List<List<int>> points = parseInput('input.txt');
  Set<int> visited = {};

  int constellations = 0;
  for (int i = 0; i < points.length; i++) {
    if (!visited.contains(i)) {
      mergeConstellations(points, i, visited);
      constellations++;
    }
  }

  print(constellations);
}
