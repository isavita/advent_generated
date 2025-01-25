
import 'dart:io';
import 'dart:math';

class Point {
  int x;
  int y;

  Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && other.x == x && other.y == y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;

  @override
  String toString() {
    return '($x, $y)';
  }
}

void main() {
  final inputGrid = readFile('input.txt');
  if (inputGrid == null) {
    print('Error reading input file.');
    return;
  }

  final antennaPositions = findAntennaPositions(inputGrid);

  int part1AntinodeCount = calculateAntinodeCountPart1(inputGrid, antennaPositions);
  print('Part 1 Antinode Count: $part1AntinodeCount');

  int part2AntinodeCount = calculateAntinodeCountPart2(inputGrid, antennaPositions);
  print('Part 2 Antinode Count: $part2AntinodeCount');
}

List<String>? readFile(String filePath) {
  try {
    return File(filePath).readAsLinesSync();
  } catch (e) {
    return null;
  }
}

Map<String, List<Point>> findAntennaPositions(List<String> grid) {
  final antennaMap = <String, List<Point>>{};
  for (int y = 0; y < grid.length; y++) {
    for (int x = 0; x < grid[y].length; x++) {
      final char = grid[y][x];
      if (char != '.') {
        antennaMap.putIfAbsent(char, () => []);
        antennaMap[char]!.add(Point(x, y));
      }
    }
  }
  return antennaMap;
}

int calculateAntinodeCountPart1(List<String> grid, Map<String, List<Point>> antennaPositions) {
  final antinodePositions = <Point>{};
  final gridWidth = grid[0].length;
  final gridHeight = grid.length;

  for (final frequency in antennaPositions.keys) {
    final antennas = antennaPositions[frequency]!;
    if (antennas.length < 2) continue;

    for (int i = 0; i < antennas.length; i++) {
      for (int j = i + 1; j < antennas.length; j++) {
        final p1 = antennas[i];
        final p2 = antennas[j];

        final ax = 2 * p1.x - p2.x;
        final ay = 2 * p1.y - p2.y;
        if (ax >= 0 && ax < gridWidth && ay >= 0 && ay < gridHeight) {
          antinodePositions.add(Point(ax, ay));
        }

        final bx = 2 * p2.x - p1.x;
        final by = 2 * p2.y - p1.y;
        if (bx >= 0 && bx < gridWidth && by >= 0 && by < gridHeight) {
          antinodePositions.add(Point(bx, by));
        }
      }
    }
  }
  return antinodePositions.length;
}


int calculateAntinodeCountPart2(List<String> grid, Map<String, List<Point>> antennaPositions) {
  final antinodePositions = <Point>{};
  final gridWidth = grid[0].length;
  final gridHeight = grid.length;

  for (final frequency in antennaPositions.keys) {
    final antennas = antennaPositions[frequency]!;
    if (antennas.length < 2) {
      continue;
    }

    for (int i = 0; i < antennas.length; i++) {
      for (int j = i + 1; j < antennas.length; j++) {
        final p1 = antennas[i];
        final p2 = antennas[j];

        int dx = (p2.x - p1.x);
        int dy = (p2.y - p1.y);
        int gcdVal = gcd(dx.abs(), dy.abs());
        dx ~/= gcdVal;
        dy ~/= gcdVal;

        int currentX = p1.x;
        int currentY = p1.y;

        while (currentX >= 0 && currentX < gridWidth && currentY >= 0 && currentY < gridHeight) {
          antinodePositions.add(Point(currentX, currentY));
          currentX -= dx;
          currentY -= dy;
        }

        currentX = p1.x;
        currentY = p1.y;
        while (currentX >= 0 && currentX < gridWidth && currentY >= 0 && currentY < gridHeight) {
          antinodePositions.add(Point(currentX, currentY));
          currentX += dx;
          currentY += dy;
        }
      }
    }
    if (antennas.length > 1) {
      antinodePositions.addAll(antennas);
    }
  }
  return antinodePositions.length;
}


int gcd(int a, int b) {
  while (b != 0) {
    int temp = b;
    b = a % b;
    a = temp;
  }
  return a;
}
