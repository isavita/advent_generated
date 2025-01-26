
import 'dart:io';

class Point {
  int x, y;
  Point(this.x, this.y);
}

void main() {
  List<String> grid = [];
  File('input.txt').readAsLinesSync().forEach((line) => grid.add(line));

  int h = grid.length, w = grid[0].length;
  Point? S, E;
  List<Point> trackCells = [];
  List<List<bool>> walls = List.generate(h, (_) => List.filled(w, false));

  for (int i = 0; i < h; i++) {
    for (int j = 0; j < w; j++) {
      String ch = grid[i][j];
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

  List<Point> dirs = [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)];

  List<List<int>> normalDistFrom(Point start) {
    List<List<int>> dist = List.generate(h, (_) => List.filled(w, -1));
    dist[start.x][start.y] = 0;
    List<Point> q = [start];
    int head = 0;
    while (head < q.length) {
      Point cur = q[head++];
      for (Point d in dirs) {
        int nx = cur.x + d.x, ny = cur.y + d.y;
        if (nx < 0 || nx >= h || ny < 0 || ny >= w || walls[nx][ny] || dist[nx][ny] != -1) {
          continue;
        }
        dist[nx][ny] = dist[cur.x][cur.y] + 1;
        q.add(Point(nx, ny));
      }
    }
    return dist;
  }

  List<List<int>> distFromS = normalDistFrom(S!);
  List<List<int>> distFromE = normalDistFrom(E!);

  if (distFromS[E.x][E.y] == -1) {
    print(0);
    return;
  }

  int normalCost = distFromS[E.x][E.y];
  bool isTrack(int x, int y) => x >= 0 && x < h && y >= 0 && y < w && !walls[x][y];

  int possibleCheats = 0;
  for (Point startPos in trackCells) {
    int sd = distFromS[startPos.x][startPos.y];
    if (sd == -1) continue;
    for (int i = -2; i <= 2; i++) {
      for (int j = -2; j <= 2; j++) {
          if (i.abs() + j.abs() > 2 || i.abs() + j.abs() == 0 ) continue;
          int endX = startPos.x + i;
          int endY = startPos.y + j;
          if (!isTrack(endX, endY)) continue;
          int ed = distFromE[endX][endY];
          if(ed == -1) continue;

          int newCost = sd + 2 + ed;
          if(normalCost - newCost >= 100)
              possibleCheats++;

      }
    }
  }

  print(possibleCheats);
}
