
import 'dart:io';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final xs = <int>[];
  final ys = <int>[];

  for (final line in lines) {
    final parts = line.trim().split(',');
    if (parts.length != 2) continue;
    final x = int.tryParse(parts[0].trim());
    final y = int.tryParse(parts[1].trim());
    if (x == null || y == null) continue;
    xs.add(x);
    ys.add(y);
  }

  final n = xs.length;
  if (n == 0) {
    print(0);
    return;
  }

  int maxArea = 1;
  for (int i = 0; i < n; i++) {
    final x1 = xs[i];
    final y1 = ys[i];
    for (int j = i + 1; j < n; j++) {
      final w = (x1 - xs[j]).abs() + 1;
      final h = (y1 - ys[j]).abs() + 1;
      final area = w * h;
      if (area > maxArea) maxArea = area;
    }
  }
  print(maxArea);
}
