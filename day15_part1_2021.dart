import 'dart:io';

class Position {
  int x, y, risk;

  Position(this.x, this.y, this.risk);
}

class PriorityQueue {
  List<Position> _list = [];

  int get length => _list.length;

  void add(Position pos) {
    _list.add(pos);
    _list.sort((a, b) => a.risk.compareTo(b.risk));
  }

  Position remove() {
    return _list.removeAt(0);
  }
}

int dijkstra(List<List<int>> grid) {
  PriorityQueue pq = PriorityQueue();
  pq.add(Position(0, 0, 0));

  int rows = grid.length;
  int cols = grid[0].length;
  List<List<int>> dist = List.generate(rows, (_) => List.filled(cols, 1 << 31 - 1));

  dist[0][0] = 0;

  List<Position> directions = [Position(1, 0, 0), Position(0, 1, 0), Position(-1, 0, 0), Position(0, -1, 0)];

  while (pq.length > 0) {
    Position curr = pq.remove();
    if (curr.x == rows - 1 && curr.y == cols - 1) {
      return curr.risk;
    }
    for (var d in directions) {
      int nx = curr.x + d.x;
      int ny = curr.y + d.y;
      if (nx >= 0 && ny >= 0 && nx < rows && ny < cols) {
        int nextRisk = curr.risk + grid[nx][ny];
        if (nextRisk < dist[nx][ny]) {
          dist[nx][ny] = nextRisk;
          pq.add(Position(nx, ny, nextRisk));
        }
      }
    }
  }
  return -1;
}

void main() {
  File file = File('input.txt');
  List<List<int>> grid = [];

  file.readAsLines().then((List<String> lines) {
    for (String line in lines) {
      List<int> row = line.runes.map((ch) => ch - '0'.codeUnitAt(0)).toList();
      grid.add(row);
    }

    print(dijkstra(grid));
  }).catchError((e) {
    print('Error reading file: $e');
  });
}