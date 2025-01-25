
import 'dart:io';
import 'dart:math';

class Point {
  int row;
  int col;

  Point(this.row, this.col);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && runtimeType == other.runtimeType && row == other.row && col == other.col;

  @override
  int get hashCode => row.hashCode ^ col.hashCode;

  @override
  String toString() {
    return '($row, $col)';
  }
}

void main() {
  final grid = File('input.txt').readAsLinesSync();
  final antennaLocations = <String, List<Point>>{};
  final rows = grid.length;
  final cols = grid.isNotEmpty ? grid[0].length : 0;

  for (var r = 0; r < rows; r++) {
    for (var c = 0; c < cols; c++) {
      final char = grid[r][c];
      if (char != '.') {
        antennaLocations.putIfAbsent(char, () => []);
        antennaLocations[char]!.add(Point(r, c));
      }
    }
  }

  final antinodeLocations = <Point>{};

  for (final frequency in antennaLocations.keys) {
    final locations = antennaLocations[frequency]!;
    for (var i = 0; i < locations.length; i++) {
      for (var j = i + 1; j < locations.length; j++) {
        final p1 = locations[i];
        final p2 = locations[j];

        final vRow = p2.row - p1.row;
        final vCol = p2.col - p1.col;

        final antinode1 = Point(p1.row - vRow, p1.col - vCol);
        final antinode2 = Point(p2.row + vRow, p2.col + vCol);

        if (antinode1.row >= 0 && antinode1.row < rows && antinode1.col >= 0 && antinode1.col < cols) {
          antinodeLocations.add(antinode1);
        }
        if (antinode2.row >= 0 && antinode2.row < rows && antinode2.col >= 0 && antinode2.col < cols) {
          antinodeLocations.add(antinode2);
        }
      }
    }
  }

  print(antinodeLocations.length);
}
