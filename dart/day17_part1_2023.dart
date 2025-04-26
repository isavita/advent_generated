import 'dart:io';
import 'dart:convert';

class State {
  final int r, c, dir, len, cost;
  State(this.r, this.c, this.dir, this.len, this.cost);
}

void main() {
  // Read grid
  final lines = File('input.txt').readAsLinesSync();
  final N = lines.length;
  final M = lines[0].length;
  final grid = List.generate(N, (i) =>
    lines[i].split('').map(int.parse).toList());

  // Directions: 0=up,1=right,2=down,3=left
  const dr = [-1, 0, 1,  0];
  const dc = [ 0, 1, 0, -1];

  // Dijkstra
  final best = <String,int>{};
  String key(int r,int c,int d,int l) => '$r,$c,$d,$l';
  final queue = <State>[];
  // start at (0,0), dir=-1,len=0,cost=0
  best[key(0,0,-1,0)] = 0;
  queue.add(State(0,0,-1,0,0));

  while (queue.isNotEmpty) {
    // pop lowest-cost
    int mi = 0;
    for (int i = 1; i < queue.length; i++) {
      if (queue[i].cost < queue[mi].cost) mi = i;
    }
    final cur = queue.removeAt(mi);
    final ck = key(cur.r, cur.c, cur.dir, cur.len);
    if (cur.cost != best[ck]) continue;

    // reached goal?
    if (cur.r == N-1 && cur.c == M-1) {
      print(cur.cost);
      return;
    }

    // consider moves
    for (int nd = 0; nd < 4; nd++) {
      if (cur.dir != -1) {
        // disallow reverse
        if ((cur.dir + 2) % 4 == nd) continue;
      }
      int nlen = (nd == cur.dir) ? cur.len + 1 : 1;
      if (nlen > 3) continue;
      final nr = cur.r + dr[nd];
      final nc = cur.c + dc[nd];
      if (nr < 0 || nr >= N || nc < 0 || nc >= M) continue;
      final ncost = cur.cost + grid[nr][nc];
      final nk = key(nr, nc, nd, nlen);
      if (best[nk] == null || ncost < best[nk]!) {
        best[nk] = ncost;
        queue.add(State(nr, nc, nd, nlen, ncost));
      }
    }
  }

  // unreachable
  print(-1);
}
