
import 'dart:io';

void main() {
  final inputFile = File('input.txt');
  final input = inputFile.readAsLinesSync();

  if (input.isEmpty) {
    print(0);
    return;
  }

  final heightMap = input
      .map((line) => line.split('').map(int.parse).toList())
      .toList();

  final rows = heightMap.length;
  final cols = heightMap[0].length;
  int totalScore = 0;

  for (int r = 0; r < rows; r++) {
    for (int c = 0; c < cols; c++) {
      if (heightMap[r][c] == 0) {
        totalScore += calculateTrailheadScore(heightMap, r, c);
      }
    }
  }

  print(totalScore);
}

int calculateTrailheadScore(List<List<int>> heightMap, int startRow, int startCol) {
  final rows = heightMap.length;
  final cols = heightMap[0].length;
  int score = 0;
  final queue = [(startRow, startCol, 0)];
  final visited = <(int, int)>{(startRow, startCol)};

  while (queue.isNotEmpty) {
    final (row, col, height) = queue.removeAt(0);

    if (heightMap[row][col] == 9) {
      score++;
    }

    final neighbors = [
      (row - 1, col), // Up
      (row + 1, col), // Down
      (row, col - 1), // Left
      (row, col + 1), // Right
    ];

    for (final neighbor in neighbors) {
      final nr = neighbor.$1;
      final nc = neighbor.$2;

      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
        if (heightMap[nr][nc] == height + 1 && !visited.contains((nr, nc))) {
          visited.add((nr, nc));
          queue.add((nr, nc, height + 1));
        }
      }
    }
  }
  return score;
}
