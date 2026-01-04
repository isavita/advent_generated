
import 'dart:io';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final points = <List<int>>[];
  for (final l in lines) {
    final t = l.trim();
    if (t.isEmpty) continue;
    final p = t.split(',').map(int.parse).toList();
    points.add(p);
  }
  final n = points.length;
  if (n < 2) {
    print('Not enough points to form circuits.');
    return;
  }

  final edges = <List<int>>[];
  for (int i = 0; i < n; i++) {
    final p1 = points[i];
    for (int j = i + 1; j < n; j++) {
      final p2 = points[j];
      final dx = p1[0] - p2[0];
      final dy = p1[1] - p2[1];
      final dz = p1[2] - p2[2];
      final d = dx * dx + dy * dy + dz * dz;
      edges.add([i, j, d]);
    }
  }

  edges.sort((a, b) => a[2] - b[2]);

  final parent = List<int>.generate(n, (i) => i);
  final size = List<int>.filled(n, 1);

  int find(int x) {
    while (parent[x] != x) {
      parent[x] = parent[parent[x]];
      x = parent[x];
    }
    return x;
  }

  void union(int x, int y) {
    int rx = find(x);
    int ry = find(y);
    if (rx == ry) return;
    if (size[rx] < size[ry]) {
      final tmp = rx;
      rx = ry;
      ry = tmp;
    }
    parent[ry] = rx;
    size[rx] += size[ry];
  }

  final limit = edges.length > 1000 ? 1000 : edges.length;
  for (int k = 0; k < limit; k++) {
    final e = edges[k];
    union(e[0], e[1]);
  }

  final roots = <int, int>{};
  for (int i = 0; i < n; i++) {
    final r = find(i);
    roots[r] = (roots[r] ?? 0) + 1;
  }

  final sizes = roots.values.toList()..sort((a, b) => b - a);
  final top3 = sizes.take(3);
  final result = top3.fold<int>(1, (a, b) => a * b);

  print(result);
}
