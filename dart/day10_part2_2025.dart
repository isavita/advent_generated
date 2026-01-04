
import 'dart:io';
import 'dart:math';

int bestResult = 0,
    rank = 0,
    numButtons = 0;
List<int> freeValues = [],
    maxPresses = [],
    pivotCol = [];
List<int> freeVars = [];
List<List<double>> matrix = [];

void main() {
  final lines = File('input.txt').readAsLinesSync();
  int total = 0;
  for (String line in lines) {
    line = line.trim();
    if (line.isEmpty) continue;
    final buttonMatches = RegExp(r'\(([^)]*)\)').allMatches(line);
    final buttons = <List<int>>[];
    for (final m in buttonMatches) {
      final s = m.group(1)!.trim();
      if (s.isEmpty) {
        buttons.add([]);
        continue;
      }
      buttons.add(s.split(',').map((e) => int.parse(e.trim())).toList());
    }
    final targetMatch = RegExp(r'\{([^}]*)\}').firstMatch(line);
    if (targetMatch != null) {
      final targets = targetMatch
          .group(1)!
          .split(',')
          .map((e) => int.parse(e.trim()))
          .toList();
      final res = solve(buttons, targets);
      if (res != -1) total += res;
    }
  }
  print(total);
}

int solve(List<List<int>> buttons, List<int> targets) {
  final n = targets.length;
  numButtons = buttons.length;
  matrix = List.generate(n, (_) => List.filled(numButtons + 1, 0.0));
  for (int j = 0; j < n; j++) matrix[j][numButtons] = targets[j].toDouble();
  for (int i = 0; i < numButtons; i++) {
    for (final j in buttons[i]) {
      if (j < n) matrix[j][i] = 1.0;
    }
  }
  pivotCol = List.filled(n, -1);
  int r = 0;
  for (int c = 0; c < numButtons && r < n; c++) {
    int mr = r;
    for (int i = r + 1; i < n; i++) {
      if (matrix[i][c].abs() > matrix[mr][c].abs()) mr = i;
    }
    if (matrix[mr][c].abs() < 1e-9) continue;
    final tmp = matrix[r];
    matrix[r] = matrix[mr];
    matrix[mr] = tmp;
    final s = matrix[r][c];
    for (int k = c; k <= numButtons; k++) matrix[r][k] /= s;
    for (int i = 0; i < n; i++) {
      if (i != r && matrix[i][c].abs() > 1e-9) {
        final f = matrix[i][c];
        for (int k = c; k <= numButtons; k++) matrix[i][k] -= f * matrix[r][k];
      }
    }
    pivotCol[r++] = c;
  }
  rank = r;
  for (int i = rank; i < n; i++) {
    if (matrix[i][numButtons].abs() > 1e-9) return -1;
  }
  final isP = List.filled(numButtons, false);
  for (int i = 0; i < rank; i++) {
    if (pivotCol[i] >= 0) isP[pivotCol[i]] = true;
  }
  freeVars = [];
  for (int i = 0; i < numButtons; i++) {
    if (!isP[i]) freeVars.add(i);
  }
  maxPresses = List.filled(numButtons, 0);
  for (int i = 0; i < numButtons; i++) {
    int m = 0x7fffffff;
    for (final j in buttons[i]) {
      if (j < n) m = min(m, targets[j]);
    }
    maxPresses[i] = (m == 0x7fffffff) ? 0 : m;
  }
  freeVars.sort((a, b) => maxPresses[a] - maxPresses[b]);
  bestResult = 0x7fffffff;
  freeValues = List.filled(freeVars.length, 0);
  enumerate(buttons, targets, 0, 0);
  return bestResult == 0x7fffffff ? -1 : bestResult;
}

void enumerate(List<List<int>> buttons, List<int> targets, int idx, int sum) {
  if (sum >= bestResult) return;
  if (idx == freeVars.length) {
    final res = List.filled(numButtons, 0);
    for (int i = 0; i < freeVars.length; i++) res[freeVars[i]] = freeValues[i];
    for (int i = rank - 1; i >= 0; i--) {
      final c = pivotCol[i];
      if (c < 0) continue;
      double v = matrix[i][numButtons];
      for (int k = c + 1; k < numButtons; k++) v -= matrix[i][k] * res[k];
      final iv = v.round();
      if ((v - iv).abs() > 1e-6 || iv < 0 || iv > maxPresses[c]) return;
      res[c] = iv;
    }
    final cur = res.reduce((a, b) => a + b);
    if (cur < bestResult) bestResult = cur;
  } else {
    final fv = freeVars[idx];
    for (int v = 0; v <= maxPresses[fv]; v++) {
      freeValues[idx] = v;
      enumerate(buttons, targets, idx + 1, sum + v);
    }
  }
}
