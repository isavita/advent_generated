
import 'dart:io';
import 'dart:collection';

class Point {
  final int r, c;
  const Point(this.r, this.c);
}

class Piece {
  final List<Point> p;
  int get n => p.length;
  const Piece(this.p);
}

Piece normalize(Piece piece) {
  if (piece.n == 0) return piece;
  int minR = piece.p.map((e) => e.r).reduce((a, b) => a < b ? a : b);
  int minC = piece.p.map((e) => e.c).reduce((a, b) => a < b ? a : b);
  List<Point> arr = piece.p
      .map((pt) => Point(pt.r - minR, pt.c - minC))
      .toList()
    ..sort((a, b) => a.r != b.r ? a.r.compareTo(b.r) : a.c.compareTo(b.c));
  return Piece(arr);
}

Piece rotate(Piece p) {
  return Piece(p.p.map((pt) => Point(pt.c, -pt.r)).toList());
}

Piece flip(Piece p) {
  return Piece(p.p.map((pt) => Point(pt.r, -pt.c)).toList());
}

bool equal(Piece a, Piece b) {
  if (a.n != b.n) return false;
  for (int i = 0; i < a.n; i++) {
    if (a.p[i].r != b.p[i].r || a.p[i].c != b.p[i].c) return false;
  }
  return true;
}

List<Piece> generateVariations(Piece base) {
  List<Piece> uniq = [];
  Piece cur = base;
  for (int i = 0; i < 4; i++) {
    Piece n = normalize(cur);
    if (!uniq.any((e) => equal(e, n))) uniq.add(n);
    Piece f = normalize(flip(cur));
    if (!uniq.any((e) => equal(e, f))) uniq.add(f);
    cur = rotate(cur);
  }
  return uniq;
}

bool canPlace(int rows, int cols, List<int> grid, Piece p, int r, int c) {
  for (int i = 0; i < p.n; i++) {
    int nr = r + p.p[i].r;
    int nc = c + p.p[i].c;
    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) return false;
    if (grid[nr * cols + nc] != 0) return false;
  }
  return true;
}

void place(int cols, List<int> grid, Piece p, int r, int c, int v) {
  for (int i = 0; i < p.n; i++) {
    grid[(r + p.p[i].r) * cols + (c + p.p[i].c)] = v;
  }
}

bool checkIslands(int rows, int cols, List<int> grid, List<int> counts,
    int arrSize, int slackIdx, List<Piece> shapes) {
  int minReal = 999999999;
  bool hasReal = false;
  for (int i = 0; i < arrSize; i++) {
    if (i != slackIdx && counts[i] > 0) {
      if (shapes[i].n < minReal) minReal = shapes[i].n;
      hasReal = true;
    }
  }
  if (!hasReal) return true;
  int avail = counts[slackIdx];
  List<bool> vis = List.filled(rows * cols, false);
  Queue<int> q = Queue<int>();
  for (int idx = 0; idx < rows * cols; idx++) {
    if (grid[idx] == 0 && !vis[idx]) {
      q.clear();
      q.add(idx);
      vis[idx] = true;
      int size = 0;
      while (q.isNotEmpty) {
        int cur = q.removeFirst();
        size++;
        int r = cur ~/ cols;
        int cc = cur % cols;
        void add(int n) {
          if (n >= 0 && n < rows * cols && grid[n] == 0 && !vis[n]) {
            vis[n] = true;
            q.add(n);
          }
        }

        add((r - 1) * cols + cc);
        add((r + 1) * cols + cc);
        add(r * cols + (cc - 1));
        add(r * cols + (cc + 1));
      }
      if (size < minReal) {
        if (avail >= size) {
          avail -= size;
        } else {
          return false;
        }
      }
    }
  }
  return true;
}

bool solveRec(
    int rows,
    int cols,
    List<int> grid,
    List<int> counts,
    int arrSize,
    List<int> ids,
    int idCount,
    List<List<Piece>> variations,
    List<int> varCounts,
    int slackIdx,
    List<Piece> shapes) {
  int empty = -1;
  for (int i = 0; i < rows * cols; i++) {
    if (grid[i] == 0) {
      empty = i;
      break;
    }
  }
  if (empty == -1) return true;
  int r = empty ~/ cols;
  int c = empty % cols;
  if (!checkIslands(rows, cols, grid, counts, arrSize, slackIdx, shapes)) {
    return false;
  }
  for (int ii = 0; ii < idCount; ii++) {
    int id = ids[ii];
    if (counts[id] > 0) {
      counts[id]--;
      for (int v = 0; v < varCounts[id]; v++) {
        Piece p = variations[id][v];
        if (canPlace(rows, cols, grid, p, r, c)) {
          place(cols, grid, p, r, c, 1);
          if (solveRec(rows, cols, grid, counts, arrSize, ids, idCount,
              variations, varCounts, slackIdx, shapes)) {
            return true;
          }
          place(cols, grid, p, r, c, 0);
        }
      }
      counts[id]++;
    }
  }
  return false;
}

void main() async {
  List<String> lines = await File('input.txt').readAsLines();
  int maxId = -1000000;
  for (String l in lines) {
    String s = l.trim();
    if (s.isNotEmpty && s.endsWith(':')) {
      int id = int.parse(s.substring(0, s.length - 1));
      if (id > maxId) maxId = id;
    }
  }
  if (maxId < 0) maxId = -1;
  int arrSize = maxId + 2;
  int slackIdx = maxId + 1;

  List<Piece> shapes = List.filled(arrSize, Piece([]));
  bool parsingShapes = true;
  int currentId = -1;
  List<String> curShape = [];
  List<String> regionLines = [];

  for (String raw in lines) {
    String s = raw.trim();
    if (s.isEmpty) continue;
    if (s.contains('x') && s.contains(':')) parsingShapes = false;
    if (parsingShapes) {
      if (s.endsWith(':')) {
        if (currentId != -1 && curShape.isNotEmpty) {
          List<Point> pts = [];
          for (int r = 0; r < curShape.length; r++) {
            for (int c = 0; c < curShape[r].length; c++) {
              if (curShape[r][c] == '#') pts.add(Point(r, c));
            }
          }
          shapes[currentId] = normalize(Piece(pts));
          curShape.clear();
        }
        currentId = int.parse(s.substring(0, s.length - 1));
      } else {
        curShape.add(s);
      }
    } else {
      regionLines.add(s);
    }
  }
  if (currentId != -1 && curShape.isNotEmpty) {
    List<Point> pts = [];
    for (int r = 0; r < curShape.length; r++) {
      for (int c = 0; c < curShape[r].length; c++) {
        if (curShape[r][c] == '#') pts.add(Point(r, c));
      }
    }
    shapes[currentId] = normalize(Piece(pts));
  }

  shapes[slackIdx] = Piece([Point(0, 0)]);

  List<List<Piece>> variations = List.generate(arrSize, (_) => []);
  List<int> varCounts = List.filled(arrSize, 0);
  for (int i = 0; i < arrSize; i++) {
    if (shapes[i].n > 0) {
      List<Piece> vars = generateVariations(shapes[i]);
      variations[i] = vars;
      varCounts[i] = vars.length;
    }
  }

  int solved = 0;
  for (String line in regionLines) {
    List<String> parts = line.split(':');
    if (parts.length == 2) {
      String dims = parts[0].trim();
      String countsStr = parts[1].trim();
      List<String> dimParts = dims.split('x');
      if (dimParts.length == 2) {
        int wx = int.parse(dimParts[0]);
        int h = int.parse(dimParts[1]);
        int gridSize = wx * h;
        List<int> pieceCounts = List.filled(arrSize, 0);
        int totalArea = 0;
        List<String> toks = countsStr.split(RegExp(r'\s+'));
        for (int idx = 0; idx < toks.length && idx < arrSize - 1; idx++) {
          int c = int.parse(toks[idx]);
          if (c > 0) {
            pieceCounts[idx] = c;
            totalArea += c * shapes[idx].n;
          }
        }
        if (totalArea <= gridSize) {
          int slack = gridSize - totalArea;
          if (slack > 0) pieceCounts[slackIdx] = slack;
          List<int> ids = [];
          for (int i = 0; i < arrSize; i++) {
            if (pieceCounts[i] > 0) ids.add(i);
          }
          ids.sort((a, b) => shapes[b].n.compareTo(shapes[a].n));
          List<int> grid = List.filled(gridSize, 0);
          if (solveRec(h, wx, grid, pieceCounts, arrSize, ids, ids.length,
              variations, varCounts, slackIdx, shapes)) {
            solved++;
          }
        }
      }
    }
  }

  print('Number of regions that fit all presents: $solved');
}
