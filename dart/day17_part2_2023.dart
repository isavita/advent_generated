import 'dart:io';

class State {
  final int r, c, dir, len, cost;
  State(this.r, this.c, this.dir, this.len, this.cost);
}

void main() {
  // Read grid
  final lines = File('input.txt').readAsLinesSync();
  final int N = lines.length, M = lines[0].length;
  final grid = List.generate(N, (i) =>
    lines[i].split('').map(int.parse).toList());

  // Directions: 0=up,1=right,2=down,3=left
  const dr = [-1, 0, 1, 0];
  const dc = [0, 1, 0, -1];
  const int minStraight = 4, maxStraight = 10;

  // Dijkstra
  final best = <String, int>{};
  String key(int r, int c, int d, int l) => '$r,$c,$d,$l';
  final queue = <State>[];
  best[key(0, 0, -1, 0)] = 0;
  queue.add(State(0, 0, -1, 0, 0));

  while (queue.isNotEmpty) {
    // pop lowest-cost state
    int mi = 0;
    for (int i = 1; i < queue.length; i++) {
      if (queue[i].cost < queue[mi].cost) mi = i;
    }
    final cur = queue.removeAt(mi);
    final curKey = key(cur.r, cur.c, cur.dir, cur.len);
    if (cur.cost != best[curKey]) continue;

    // check goal with min run length
    if (cur.r == N - 1 &&
        cur.c == M - 1 &&
        cur.len >= minStraight) {
      print(cur.cost);
      return;
    }

    // generate moves
    for (int nd = 0; nd < 4; nd++) {
      // no reverse
      if (cur.dir != -1 && nd == (cur.dir + 2) % 4) continue;
      // require minStraight before turning
      if (cur.dir != -1 && nd != cur.dir && cur.len < minStraight) continue;
      // compute new run length
      final int newLen = (nd == cur.dir) ? cur.len + 1 : 1;
      if (newLen > maxStraight) continue;

      final int nr = cur.r + dr[nd];
      final int nc = cur.c + dc[nd];
      if (nr < 0 || nr >= N || nc < 0 || nc >= M) continue;

      final int newCost = cur.cost + grid[nr][nc];
      final newKey = key(nr, nc, nd, newLen);
      if (best[newKey] == null || newCost < best[newKey]!) {
        best[newKey] = newCost;
        queue.add(State(nr, nc, nd, newLen, newCost));
      }
    }
  }

  // no solution found
  print(-1);
}
