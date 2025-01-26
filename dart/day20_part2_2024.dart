
import 'dart:io';

class Point {
  int x, y;
  Point(this.x, this.y);
}

void main() {
  final f = File('input.txt');
  final lines = f.readAsLinesSync();
  final h = lines.length;
  final w = lines[0].length;
  Point? S, E;
  final walls = List.generate(h, (_) => List.filled(w, false));
  final trackCells = <Point>[];

  for (int i = 0; i < h; i++) {
    for (int j = 0; j < w; j++) {
      final ch = lines[i][j];
      if (ch == 'S') {
        S = Point(i, j);
      } else if (ch == 'E') {
        E = Point(i, j);
      }
      if (ch == '#') {
        walls[i][j] = true;
      } else {
        trackCells.add(Point(i, j));
      }
    }
  }

  final dirs = [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)];

  bool isTrack(int x, int y) {
    return x >= 0 && x < h && y >= 0 && y < w && !walls[x][y];
  }

  List<List<int>> normalDistFrom(Point start) {
    final dist = List.generate(h, (_) => List.filled(w, -1));
    dist[start.x][start.y] = 0;
    final q = [start];
    int head = 0;
    while (head < q.length) {
      final cur = q[head++];
      for (final d in dirs) {
        final nx = cur.x + d.x;
        final ny = cur.y + d.y;
        if (nx < 0 || nx >= h || ny < 0 || ny >= w) continue;
        if (walls[nx][ny]) continue;
        if (dist[nx][ny] < 0) {
          dist[nx][ny] = dist[cur.x][cur.y] + 1;
          q.add(Point(nx, ny));
        }
      }
    }
    return dist;
  }

  final distFromS = normalDistFrom(S!);
  final distFromE = normalDistFrom(E!);
  if (distFromS[E.x][E.y] < 0) {
    print(0);
    return;
  }
  final normalCost = distFromS[E.x][E.y];

  final cheats = <List<int>, int>{};

  for (final startPos in trackCells) {
    final sd = distFromS[startPos.x][startPos.y];
    if (sd < 0) continue;

    final distC = List.generate(h, (_) => List.filled(w, -1));
    distC[startPos.x][startPos.y] = 0;
    final q = [startPos];
    int head = 0;
    while (head < q.length) {
      final cur = q[head++];
      final steps = distC[cur.x][cur.y];
      if (steps == 20) continue;
      for (final d in dirs) {
        final nx = cur.x + d.x;
        final ny = cur.y + d.y;
        if (nx < 0 || nx >= h || ny < 0 || ny >= w) continue;
        if (distC[nx][ny] < 0) {
          distC[nx][ny] = steps + 1;
          q.add(Point(nx, ny));
        }
      }
    }

    for (int x = 0; x < h; x++) {
      for (int y = 0; y < w; y++) {
        final s = distC[x][y];
        if (s > 0 && s <= 20 && isTrack(x, y)) {
          final ed = distFromE[x][y];
          if (ed < 0) continue;
          final cost = sd + s + ed;
          if (cost < normalCost) {
            final key = [startPos.x, startPos.y, x, y];
            final old = cheats[key];
            if (old == null || cost < old) {
              cheats[key] = cost;
            }
          }
        }
      }
    }
  }

  int count = 0;
  for (final cost in cheats.values) {
    final saving = normalCost - cost;
    if (saving >= 100) {
      count++;
    }
  }
  print(count);
}
