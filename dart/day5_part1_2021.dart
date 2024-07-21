
import 'dart:io';

void main() async {
  // Read input from the file
  final lines = await File('input.txt').readAsLines();

  // Create a map to count overlaps
  Map<String, int> grid = {};

  for (var line in lines) {
    // Parse the line to get coordinates
    var parts = line.split(' -> ');
    var start = parts[0].split(',').map(int.parse).toList();
    var end = parts[1].split(',').map(int.parse).toList();

    int x1 = start[0], y1 = start[1];
    int x2 = end[0], y2 = end[1];

    // Only consider horizontal and vertical lines
    if (x1 == x2) {
      // Vertical line
      int minY = y1 < y2 ? y1 : y2;
      int maxY = y1 > y2 ? y1 : y2;
      for (int y = minY; y <= maxY; y++) {
        String key = '$x1,$y';
        grid[key] = (grid[key] ?? 0) + 1;
      }
    } else if (y1 == y2) {
      // Horizontal line
      int minX = x1 < x2 ? x1 : x2;
      int maxX = x1 > x2 ? x1 : x2;
      for (int x = minX; x <= maxX; x++) {
        String key = '$x,$y1';
        grid[key] = (grid[key] ?? 0) + 1;
      }
    }
  }

  // Count the number of points with at least two overlaps
  int overlapCount = grid.values.where((count) => count >= 2).length;

  // Print the result
  print('Number of points where at least two lines overlap: $overlapCount');
}
