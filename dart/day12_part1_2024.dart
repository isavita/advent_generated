
import 'dart:io';

void main() {
  final input = File('input.txt').readAsLinesSync();
  final grid = input.map((line) => line.split('')).toList();
  final height = grid.length;
  final width = grid[0].length;
  final visited = List.generate(height, (_) => List.generate(width, (_) => false));
  int totalPrice = 0;

  for (int r = 0; r < height; r++) {
    for (int c = 0; c < width; c++) {
      if (!visited[r][c]) {
        final regionType = grid[r][c];
        final regionCells = <Point<int>>[];
        exploreRegion(grid, visited, r, c, regionType, regionCells);

        if (regionCells.isNotEmpty) {
          final area = regionCells.length;
          final perimeter = calculatePerimeter(grid, regionCells, regionType);
          totalPrice += area * perimeter;
        }
      }
    }
  }
  print(totalPrice);
}

void exploreRegion(
  List<List<String>> grid,
  List<List<bool>> visited,
  int r,
  int c,
  String regionType,
  List<Point<int>> regionCells,
) {
  if (r < 0 || r >= grid.length || c < 0 || c >= grid[0].length || visited[r][c] || grid[r][c] != regionType) {
    return;
  }

  visited[r][c] = true;
  regionCells.add(Point(r, c));

  exploreRegion(grid, visited, r + 1, c, regionType, regionCells);
  exploreRegion(grid, visited, r - 1, c, regionType, regionCells);
  exploreRegion(grid, visited, r, c + 1, regionType, regionCells);
  exploreRegion(grid, visited, r, c - 1, regionType, regionCells);
}

int calculatePerimeter(List<List<String>> grid, List<Point<int>> regionCells, String regionType) {
  int perimeter = 0;
  final height = grid.length;
  final width = grid[0].length;

  for (final cell in regionCells) {
    final r = cell.r;
    final c = cell.c;

    // Check up
    if (r - 1 < 0 || grid[r - 1][c] != regionType) {
      perimeter++;
    }
    // Check down
    if (r + 1 >= height || grid[r + 1][c] != regionType) {
      perimeter++;
    }
    // Check left
    if (c - 1 < 0 || grid[r][c - 1] != regionType) {
      perimeter++;
    }
    // Check right
    if (c + 1 >= width || grid[r][c + 1] != regionType) {
      perimeter++;
    }
  }
  return perimeter;
}

class Point<T extends num> {
  final T r;
  final T c;
  Point(this.r, this.c);
}
