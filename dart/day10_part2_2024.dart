
import 'dart:io';

void main() {
  final inputFile = File('input.txt');
  final input = inputFile.readAsLinesSync();

  final heightMap = input
      .map((line) => line.split('').map(int.parse).toList())
      .toList();

  int part1Sum = 0;
  int part2Sum = 0;

  for (int r = 0; r < heightMap.length; r++) {
    for (int c = 0; c < heightMap[r].length; c++) {
      if (heightMap[r][c] == 0) {
        part1Sum += calculateTrailheadScore(r, c, heightMap);
        part2Sum += calculateTrailheadRating(r, c, heightMap);
      }
    }
  }

  print(part1Sum);
  print(part2Sum);
}

int calculateTrailheadScore(int startRow, int startCol, List<List<int>> heightMap) {
  final rows = heightMap.length;
  final cols = heightMap[0].length;
  final visited = List.generate(rows, (i) => List.filled(cols, false));

  int dfsScore(int r, int c) {
    if (!isValid(r, c, heightMap, visited)) {
      return 0;
    }
    if (heightMap[r][c] == 9) {
      return 1;
    }
    visited[r][c] = true;
    int count = 0;
    final neighbors = getNeighbors(r, c);
    for (final neighbor in neighbors) {
      final nr = neighbor[0];
      final nc = neighbor[1];
      if (isValid(nr, nc, heightMap, visited) && heightMap[nr][nc] == heightMap[r][c] + 1) {
        count += dfsScore(nr, nc);
      }
    }
    return count;
  }

  return dfsScore(startRow, startCol);
}

int calculateTrailheadRating(int startRow, int startCol, List<List<int>> heightMap) {
  final rows = heightMap.length;
  final cols = heightMap[0].length;

  int dfsRating(int r, int c) {
    if (!isValidRating(r, c, heightMap)) {
      return 0;
    }
    if (heightMap[r][c] == 9) {
      return 1;
    }
    int pathCount = 0;
    final neighbors = getNeighbors(r, c);
    for (final neighbor in neighbors) {
      final nr = neighbor[0];
      final nc = neighbor[1];
      if (isValidRating(nr, nc, heightMap) && heightMap[nr][nc] == heightMap[r][c] + 1) {
        pathCount += dfsRating(nr, nc);
      }
    }
    return pathCount;
  }

  return dfsRating(startRow, startCol);
}


bool isValid(int r, int c, List<List<int>> heightMap, List<List<bool>> visited) {
  final rows = heightMap.length;
  final cols = heightMap[0].length;
  return r >= 0 && r < rows && c >= 0 && c < cols && !visited[r][c];
}

bool isValidRating(int r, int c, List<List<int>> heightMap) {
  final rows = heightMap.length;
  final cols = heightMap[0].length;
  return r >= 0 && r < rows && c >= 0 && c < cols;
}


List<List<int>> getNeighbors(int r, int c) {
  return [
    [r - 1, c], // Up
    [r + 1, c], // Down
    [r, c - 1], // Left
    [r, c + 1], // Right
  ];
}
