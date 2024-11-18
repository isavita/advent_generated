
import 'dart:io';
import 'dart:collection';

void main() {
  final map = parse();
  print(bfs(map));
}

class Point {
  final int x, y;
  Point(this.x, this.y);

  @override
  bool operator ==(Object other) => 
    other is Point && x == other.x && y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;

  List<Point> neighbours() => [
    Point(x, y + 1),
    Point(x + 1, y),
    Point(x, y - 1),
    Point(x - 1, y),
  ];
}

class MapData {
  final int xMax, yMax;
  final Map<Point, String> grid;
  final Point aa, zz;
  final Map<Point, Point> teleport;
  final Map<Point, bool> isOuter;

  MapData({
    required this.xMax,
    required this.yMax,
    required this.grid,
    required this.aa,
    required this.zz,
    required this.teleport,
    required this.isOuter,
  });
}

MapData parse() {
  final grid = <Point, String>{};
  int xMax = 0, yMax = 0;

  final lines = File('input.txt').readAsLinesSync();
  xMax = lines.length;
  yMax = lines.map((line) => line.length).reduce((a, b) => a > b ? a : b);

  for (int i = 0; i < lines.length; i++) {
    for (int j = 0; j < lines[i].length; j++) {
      grid[Point(i, j)] = lines[i][j];
    }
  }

  Point? aa, zz;
  final isOuter = <Point, bool>{};
  final teleport = <Point, Point>{};
  final cache = <String, Point>{};

  for (int i = 0; i < xMax; i++) {
    for (int j = 0; j < yMax; j++) {
      final p = Point(i, j);
      final c = grid[p];

      if (!_isLetter(c!)) continue;

      final portalInfo = _extractPortal(grid, p);
      if (portalInfo == null) continue;

      final (portalName, portalPoint) = portalInfo;

      if (portalName == 'AA') {
        aa = portalPoint;
        isOuter[portalPoint] = true;
        continue;
      }

      if (portalName == 'ZZ') {
        zz = portalPoint;
        isOuter[portalPoint] = true;
        continue;
      }

      if (cache.containsKey(portalName)) {
        teleport[portalPoint] = cache[portalName]!;
        teleport[cache[portalName]!] = portalPoint;
      } else {
        cache[portalName] = portalPoint;
      }

      isOuter[portalPoint] = j == 0 || i == 0 || i == xMax - 2 || j == yMax - 2;
    }
  }

  return MapData(
    xMax: xMax,
    yMax: yMax,
    grid: grid,
    aa: aa!,
    zz: zz!,
    teleport: teleport,
    isOuter: isOuter,
  );
}

(String, Point)? _extractPortal(Map<Point, String> grid, Point p) {
  final c1 = grid[p];
  
  final right = Point(p.x + 1, p.y);
  final left = Point(p.x - 1, p.y);
  final down = Point(p.x, p.y + 1);
  final up = Point(p.x, p.y - 1);
  final rightPoint = Point(p.x + 2, p.y);
  final leftPoint = Point(p.x - 1, p.y);
  final downPoint = Point(p.x, p.y + 2);
  final upPoint = Point(p.x, p.y - 1);

  if (_isLetter(grid[right] ?? '')) {
    final portalName = c1! + grid[right]!;
    
    if (grid[rightPoint] == '.') return (portalName, rightPoint);
    if (grid[leftPoint] == '.') return (portalName, leftPoint);
  }

  if (_isLetter(grid[down] ?? '')) {
    final portalName = c1! + grid[down]!;
    
    if (grid[downPoint] == '.') return (portalName, downPoint);
    if (grid[upPoint] == '.') return (portalName, upPoint);
  }

  return null;
}

bool _isLetter(String c) => 
  c.isNotEmpty && c.codeUnitAt(0) >= 65 && c.codeUnitAt(0) <= 90;

int bfs(MapData m) {
  final discovered = <Point>{};
  final toDo = Queue<Point>();

  discovered.add(m.aa);
  toDo.add(m.aa);

  var depth = 0;

  while (toDo.isNotEmpty) {
    for (var levelSize = toDo.length; levelSize > 0; levelSize--) {
      final curr = toDo.removeFirst();

      if (curr == m.zz) return depth;

      for (final n in curr.neighbours()) {
        final dest = m.grid[n];

        switch (dest) {
          case '#':
            continue;
          case '.':
            if (!discovered.contains(n)) {
              discovered.add(n);
              toDo.add(n);
            }
            break;
          default:
            if (_isLetter(dest ?? '')) {
              final next = m.teleport[curr];
              if (next != null && !discovered.contains(next)) {
                discovered.add(next);
                toDo.add(next);
              }
            }
        }
      }
    }
    depth++;
  }

  return -1;
}
