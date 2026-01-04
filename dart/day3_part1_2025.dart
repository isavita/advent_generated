
import 'dart:io';

int calcMaxJolt(String bank) {
  for (int d1 = 9; d1 >= 0; d1--) {
    final idx = bank.indexOf('$d1');
    if (idx != -1 && idx < bank.length - 1) {
      int maxD2 = -1;
      for (int i = idx + 1; i < bank.length; i++) {
        final c = bank[i];
        if (c.codeUnitAt(0) >= 48 && c.codeUnitAt(0) <= 57) {
          final v = c.codeUnitAt(0) - 48;
          if (v > maxD2) maxD2 = v;
        }
      }
      if (maxD2 != -1) return d1 * 10 + maxD2;
    }
  }
  return 0;
}

void main() {
  final lines = File('input.txt').readAsLinesSync()
      .map((l) => l.trim())
      .where((l) => l.isNotEmpty);
  int total = 0;
  for (final l in lines) total += calcMaxJolt(l);
  print(total);
}
