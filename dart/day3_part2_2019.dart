
import 'dart:io';

void main() async {
  // Read input from the file
  final lines = await File('input.txt').readAsLines();
  final wire1Path = lines[0].split(',');
  final wire2Path = lines[1].split(',');

  // Create a map to store the coordinates and steps of wire 1
  final wire1Coords = <String, int>{};
  traceWire(wire1Path, wire1Coords);

  // Create a map to store the coordinates and steps of wire 2
  final wire2Coords = <String, int>{};
  traceWire(wire2Path, wire2Coords);

  // Find intersections
  final intersections = wire1Coords.keys.where((coord) => wire2Coords.containsKey(coord));

  // Part One: Find the closest intersection by Manhattan distance
  int closestDistance = intersections
      .map((coord) {
        final parts = coord.split(',').map(int.parse).toList();
        return parts[0].abs() + parts[1].abs();
      })
      .reduce((a, b) => a < b ? a : b);

  print('Part One: Closest Manhattan distance is $closestDistance');

  // Part Two: Find the intersection with the minimum combined steps
  int fewestSteps = intersections
      .map((coord) => wire1Coords[coord]! + wire2Coords[coord]!)
      .reduce((a, b) => a < b ? a : b);

  print('Part Two: Fewest combined steps is $fewestSteps');
}

void traceWire(List<String> path, Map<String, int> coords) {
  int x = 0, y = 0, steps = 0;

  for (var move in path) {
    final direction = move[0];
    final distance = int.parse(move.substring(1));

    for (int i = 0; i < distance; i++) {
      switch (direction) {
        case 'R':
          x++;
          break;
        case 'L':
          x--;
          break;
        case 'U':
          y++;
          break;
        case 'D':
          y--;
          break;
      }
      steps++;
      final coord = '$x,$y';
      // Only record the first time we visit this coordinate
      if (!coords.containsKey(coord)) {
        coords[coord] = steps;
      }
    }
  }
}
