
import 'dart:io';

void main() {
  const TOTAL = 100, START = 50, TARGET = 0;
  int pos = START, zeros = 0;

  for (final line in File('input.txt').readAsLinesSync()) {
    final m = RegExp(r'^([LR])(\d+)$').firstMatch(line.trim());
    if (m == null) continue;
    pos = (pos + (m.group(1) == 'R' ? 1 : -1) * int.parse(m.group(2)!)) % TOTAL;
    if (pos < 0) pos += TOTAL;
    if (pos == TARGET) zeros++;
  }
  print(zeros);
}
