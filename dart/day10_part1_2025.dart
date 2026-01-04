
import 'dart:io';
import 'dart:math';

void main() {
  final file = File('input.txt');
  if (!file.existsSync()) return;
  final lines = file.readAsLinesSync();
  int total = 0;
  for (final line in lines) {
    final l = line.trim();
    if (l.isEmpty) continue;
    total += _solveMachine(l);
  }
  print(total);
}

int _solveMachine(String line) {
  final start = line.indexOf('[');
  final end = line.indexOf(']');
  if (start == -1 || end == -1) return 0;
  final target = line.substring(start + 1, end);
  final n = target.length;
  final b = List.filled(n, 0);
  for (int i = 0; i < n; i++) b[i] = target[i] == '#' ? 1 : 0;

  final buttons = <List<int>>[];
  final reg = RegExp(r'\((.*?)\)');
  for (final m in reg.allMatches(line)) {
    final parts = m.group(1)!.split(',');
    final vec = List.filled(n, 0);
    for (final p in parts) {
      final idx = int.tryParse(p.trim());
      if (idx != null && idx >= 0 && idx < n) vec[idx] = 1;
    }
    buttons.add(vec);
  }
  final m = buttons.length;
  if (m == 0) return 0;

  final mat = List.generate(n, (_) => List.filled(m + 1, 0));
  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) mat[i][j] = buttons[j][i];
  }
  for (int i = 0; i < n; i++) mat[i][m] = b[i];

  final pivotCol = List.filled(n, -1);
  int r = 0;
  for (int c = 0; c < m && r < n; c++) {
    int sel = r;
    while (sel < n && mat[sel][c] == 0) sel++;
    if (sel == n) continue;
    final tmp = mat[r];
    mat[r] = mat[sel];
    mat[sel] = tmp;
    pivotCol[r] = c;
    for (int i = 0; i < n; i++) {
      if (i != r && mat[i][c] == 1) {
        for (int j = c; j <= m; j++) mat[i][j] ^= mat[r][j];
      }
    }
    r++;
  }
  for (int i = r; i < n; i++) if (mat[i][m] == 1) return 0;

  final isPivot = List.filled(m, false);
  for (int i = 0; i < r; i++) isPivot[pivotCol[i]] = true;
  final free = <int>[];
  for (int j = 0; j < m; j++) if (!isPivot[j]) free.add(j);
  final f = free.length;
  int best = m + 1;

  for (int mask = 0; mask < (1 << f); mask++) {
    int cost = 0;
    final x = List.filled(m, 0);
    for (int j = 0; j < f; j++) {
      if ((mask >> j) & 1 == 1) {
        x[free[j]] = 1;
        cost++;
      }
    }
    for (int i = 0; i < r; i++) {
      int val = mat[i][m];
      for (int j = 0; j < f; j++) {
        if (mat[i][free[j]] == 1 && x[free[j]] == 1) val ^= 1;
      }
      if (val == 1) cost++;
    }
    if (cost < best) best = cost;
  }
  return best;
}
