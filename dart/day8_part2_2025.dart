
import 'dart:io';

class Box {
  final int x, y, z;
  Box(this.x, this.y, this.z);
}

class Edge implements Comparable<Edge> {
  final int u, v;
  final int distSq;
  Edge(this.u, this.v, this.distSq);
  @override
  int compareTo(Edge other) => distSq.compareTo(other.distSq);
}

class DSU {
  final List<int> parent;
  int componentCount;
  DSU(int n)
      : parent = List.generate(n, (i) => i),
        componentCount = n;
  int find(int i) {
    var root = i;
    while (parent[root] != root) root = parent[root];
    while (parent[i] != root) {
      final next = parent[i];
      parent[i] = root;
      i = next;
    }
    return root;
  }
  bool union(int u, int v) {
    final rootU = find(u);
    final rootV = find(v);
    if (rootU != rootV) {
      parent[rootU] = rootV;
      componentCount--;
      return true;
    }
    return false;
  }
}

void main() {
  final boxes = File('input.txt')
      .readAsLinesSync()
      .map((l) {
        final p = l.split(',').map(int.parse).toList();
        return Box(p[0], p[1], p[2]);
      })
      .toList();
  final n = boxes.length;
  if (n < 2) return;
  final edges = <Edge>[];
  for (int i = 0; i < n; i++) {
    for (int j = i + 1; j < n; j++) {
      final b1 = boxes[i], b2 = boxes[j];
      final dx = b1.x - b2.x, dy = b1.y - b2.y, dz = b1.z - b2.z;
      edges.add(Edge(i, j, dx * dx + dy * dy + dz * dz));
    }
  }
  edges.sort();
  final dsu = DSU(n);
  for (final e in edges) {
    if (dsu.union(e.u, e.v) && dsu.componentCount == 1) {
      print(boxes[e.u].x * boxes[e.v].x);
      return;
    }
  }
}
