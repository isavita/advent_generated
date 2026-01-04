
import 'dart:io';

bool isInvalid(int n) {
  final s = n.toString();
  final len = s.length;
  for (int d = 1; d <= len ~/ 2; d++) {
    if (len % d != 0) continue;
    bool ok = true;
    for (int i = d; ok && i < len; i += d) {
      for (int j = 0; j < d; j++) {
        if (s[j] != s[i + j]) {
          ok = false;
          break;
        }
      }
    }
    if (ok) return true;
  }
  return false;
}

void main() {
  final input = File('input.txt').readAsStringSync().replaceAll(RegExp(r'\s+'), '');
  if (input.isEmpty) return;
  final ranges = input.split(',');
  final seen = <int>{};
  for (final r in ranges) {
    if (r.isEmpty) continue;
    final parts = r.split('-');
    if (parts.length != 2) continue;
    final start = int.tryParse(parts[0]);
    final end = int.tryParse(parts[1]);
    if (start == null || end == null) continue;
    for (int id = start; id <= end; id++) {
      if (isInvalid(id)) seen.add(id);
    }
  }
  print(seen.fold<int>(0, (a, b) => a + b));
}
