
import 'dart:collection';
import 'dart:io';

class Point {
  int x, y;
  Point(this.x, this.y);
}

void main() {
  final redTiles = <Point>[];
  try {
    for (final l in File('input.txt').readAsLinesSync()) {
      final p = l.split(',');
      if (p.length >= 2) {
        redTiles.add(Point(int.parse(p[0]), int.parse(p[1])));
      }
    }
  } catch (_) {}
  if (redTiles.isEmpty) return;

  final uniqueX = SplayTreeSet<int>();
  final uniqueY = SplayTreeSet<int>();
  for (final p in redTiles) {
    uniqueX.add(p.x);
    uniqueY.add(p.y);
  }
  final sortedX = uniqueX.toList();
  final sortedY = uniqueY.toList();
  final xMap = <int, int>{};
  final yMap = <int, int>{};
  for (int i = 0; i < sortedX.length; i++) xMap[sortedX[i]] = i;
  for (int j = 0; j < sortedY.length; j++) yMap[sortedY[j]] = j;

  final gridW = 2 * sortedX.length - 1;
  final gridH = 2 * sortedY.length - 1;
  final grid = List.generate(gridW, (_) => List.filled(gridH, false));

  for (int i = 0; i < redTiles.length; i++) {
    final p1 = redTiles[i];
    final p2 = redTiles[(i + 1) % redTiles.length];
    final ix1 = xMap[p1.x]!, iy1 = yMap[p1.y]!;
    final ix2 = xMap[p2.x]!, iy2 = yMap[p2.y]!;
    for (int x = 2 * ix1; x <= 2 * ix2; x++) {
      for (int y = 2 * iy1; y <= 2 * iy2; y++) {
        grid[x][y] = true;
      }
    }
  }

  for (int i = 0; i < sortedX.length - 1; i++) {
    final midX = (sortedX[i] + sortedX[i + 1]) / 2;
    final crossingY = <int>[];
    for (int k = 0; k < redTiles.length; k++) {
      final p1 = redTiles[k];
      final p2 = redTiles[(k + 1) % redTiles.length];
      if (p1.y == p2.y &&
          ((p1.x < midX && midX < p2.x) || (p2.x < midX && midX < p1.x))) {
        crossingY.add(p1.y);
      }
    }
    crossingY.sort();
    var inside = false;
    var crossingIdx = 0;
    for (int j = 0; j < sortedY.length - 1; j++) {
      final midY = (sortedY[j] + sortedY[j + 1]) / 2;
      while (crossingIdx < crossingY.length && crossingY[crossingIdx] < midY) {
        inside = !inside;
        crossingIdx++;
      }
      if (inside) {
        for (int gx = 2 * i; gx <= 2 * i + 2; gx++) {
          for (int gy = 2 * j; gy <= 2 * j + 2; gy++) {
            grid[gx][gy] = true;
          }
        }
      }
    }
  }

  final pref = List.generate(gridW + 1, (_) => List.filled(gridH + 1, 0));
  for (int i = 0; i < gridW; i++) {
    for (int j = 0; j < gridH; j++) {
      pref[i + 1][j + 1] =
          (grid[i][j] ? 1 : 0) +
          pref[i][j + 1] +
          pref[i + 1][j] -
          pref[i][j];
    }
  }

  int maxArea = 0;
  for (int i = 0; i < redTiles.length; i++) {
    for (int j = 0; j < redTiles.length; j++) {
      final p1 = redTiles[i];
      final p2 = redTiles[j];
      final gx1 = 2 * xMap[p1.x]!;
      final gy1 = 2 * yMap[p1.y]!;
      final gx2 = 2 * xMap[p2.x]!;
      final gy2 = 2 * yMap[p2.y]!;
      final minGX = gx1 < gx2 ? gx1 : gx2;
      final maxGX = gx1 > gx2 ? gx1 : gx2;
      final minGY = gy1 < gy2 ? gy1 : gy2;
      final maxGY = gy1 > gy2 ? gy1 : gy2;
      final requiredCount = (maxGX - minGX + 1) * (maxGY - minGY + 1);
      final actualCount = pref[maxGX + 1][maxGY + 1] -
          pref[minGX][maxGY + 1] -
          pref[maxGX + 1][minGY] +
          pref[minGX][minGY];
      if (actualCount == requiredCount) {
        final area = ((p1.x - p2.x).abs() + 1) * ((p1.y - p2.y).abs() + 1);
        if (area > maxArea) maxArea = area;
      }
    }
  }
  print(maxArea);
}
