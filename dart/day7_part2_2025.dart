
import 'dart:io';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  if (lines.isEmpty) return;
  final R = lines.length, C = lines[0].length;
  int sr = -1, sc = -1;
  for (int r = 0; r < R && sr == -1; r++) {
    final c = lines[r].indexOf('S');
    if (c != -1) { sr = r; sc = c; }
  }
  if (sr == -1) { print(0); return; }

  final dp = List.generate(R, (_) => List.filled(C, 0), growable: false);
  dp[sr][sc] = 1;
  int total = 0;

  for (int r = 0; r < R; r++) {
    for (int c = 0; c < C; c++) {
      final v = dp[r][c];
      if (v == 0) continue;
      final nr = r + 1;
      if (nr == R) {
        total += v;
        continue;
      }
      final ch = lines[nr][c];
      if (ch == '^') {
        if (c - 1 < 0) total += v; else dp[nr][c - 1] += v;
        if (c + 1 >= C) total += v; else dp[nr][c + 1] += v;
      } else {
        dp[nr][c] += v;
      }
    }
  }
  print(total);
}
