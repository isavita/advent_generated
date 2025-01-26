
import 'dart:io';
import 'dart:math';

class Node {
  int x, y, d, cost;
  Node(this.x, this.y, this.d, this.cost);
}

class MinHeap {
  List<Node> a = [];

  void push(Node v) {
    a.add(v);
    up(a.length - 1);
  }

  Node pop() {
    Node v = a[0];
    a[0] = a.last;
    a.removeLast();
    down(0);
    return v;
  }

  void up(int i) {
    while (i > 0) {
      int p = (i - 1) ~/ 2;
      if (a[p].cost <= a[i].cost) break;
      Node temp = a[p];
      a[p] = a[i];
      a[i] = temp;
      i = p;
    }
  }

  void down(int i) {
    while (true) {
      int l = 2 * i + 1;
      int r = 2 * i + 2;
      int small = i;
      if (l < a.length && a[l].cost < a[small].cost) small = l;
      if (r < a.length && a[r].cost < a[small].cost) small = r;
      if (small == i) break;
      Node temp = a[i];
      a[i] = a[small];
      a[small] = temp;
      i = small;
    }
  }
}

void main() {
  List<String> grid = File('input.txt').readAsLinesSync();
  int n = grid.length, m = grid[0].length;
  int sx = 0, sy = 0, ex = 0, ey = 0;

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      if (grid[i][j] == 'S') {
        sx = i;
        sy = j;
      } else if (grid[i][j] == 'E') {
        ex = i;
        ey = j;
      }
    }
  }

  List<int> dx = [-1, 0, 1, 0];
  List<int> dy = [0, 1, 0, -1];
  List<List<List<int>>> dist = List.generate(
      n, (_) => List.generate(m, (_) => List.filled(4, 2147483647)));
  dist[sx][sy][1] = 0;

  MinHeap h = MinHeap();
  h.push(Node(sx, sy, 1, 0));

  while (h.a.isNotEmpty) {
    Node u = h.pop();
    if (dist[u.x][u.y][u.d] < u.cost) continue;
    if (u.x == ex && u.y == ey) {
      print(u.cost);
      return;
    }

    for (int ndir in [(u.d + 1) % 4, (u.d + 3) % 4]) {
      int nc = u.cost + 1000;
      if (nc < dist[u.x][u.y][ndir]) {
        dist[u.x][u.y][ndir] = nc;
        h.push(Node(u.x, u.y, ndir, nc));
      }
    }

    int nx = u.x + dx[u.d], ny = u.y + dy[u.d];
    if (nx >= 0 &&
        nx < n &&
        ny >= 0 &&
        ny < m &&
        grid[nx][ny] != '#') {
      int nc = u.cost + 1;
      if (nc < dist[nx][ny][u.d]) {
        dist[nx][ny][u.d] = nc;
        h.push(Node(nx, ny, u.d, nc));
      }
    }
  }
}
