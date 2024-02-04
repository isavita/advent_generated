
import 'dart:io';

class Position {
  int x, y, risk;

  Position(this.x, this.y, this.risk);
}

class PriorityQueue {
  List<Position> pq = [];

  int get length => pq.length;

  void push(Position x) {
    pq.add(x);
  }

  Position pop() {
    pq.sort((a, b) => a.risk.compareTo(b.risk));
    return pq.removeAt(0);
  }
}

int dijkstra(List<List<int>> grid) {
  PriorityQueue pq = PriorityQueue();
  pq.push(Position(0, 0, 0));

  int rows = grid.length;
  int cols = grid[0].length;
  List<List<int>> dist = List.generate(rows, (i) => List.filled(cols, 1<<31 - 1));

  dist[0][0] = 0;

  List<Position> directions = [Position(1, 0, 0), Position(0, 1, 0), Position(-1, 0, 0), Position(0, -1, 0)];

  while (pq.length > 0) {
    Position curr = pq.pop();
    if (curr.x == rows - 1 && curr.y == cols - 1) {
      return curr.risk;
    }
    for (Position d in directions) {
      int nx = curr.x + d.x;
      int ny = curr.y + d.y;
      if (nx >= 0 && ny >= 0 && nx < rows && ny < cols) {
        int nextRisk = curr.risk + grid[nx][ny];
        if (nextRisk < dist[nx][ny]) {
          dist[nx][ny] = nextRisk;
          pq.push(Position(nx, ny, nextRisk));
        }
      }
    }
  }
  return -1;
}

List<List<int>> extendGrid(List<List<int>> initialGrid) {
  int rows = initialGrid.length;
  int cols = initialGrid[0].length;
  List<List<int>> extendedGrid = List.generate(rows * 5, (i) => List.filled(cols * 5, 0));

  for (int i = 0; i < rows * 5; i++) {
    for (int j = 0; j < cols * 5; j++) {
      int newRisk = initialGrid[i % rows][j % cols] + i ~/ rows + j ~/ cols;
      if (newRisk > 9) {
        newRisk -= 9;
      }
      extendedGrid[i][j] = newRisk;
    }
  }
  return extendedGrid;
}

void main() {
  File file = File('input.txt');
  List<List<int>> initialGrid = [];

  try {
    List<String> lines = file.readAsLinesSync();
    for (String line in lines) {
      List<int> row = line.runes.map((ch) => ch - '0'.codeUnitAt(0)).toList();
      initialGrid.add(row);
    }

    List<List<int>> extendedGrid = extendGrid(initialGrid);

    print(dijkstra(extendedGrid));
  } catch (e) {
    print('Error reading file: $e');
  }
}
