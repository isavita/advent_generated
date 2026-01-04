
import 'dart:io';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  BigInt total = BigInt.zero;
  const k = 12;

  for (final raw in lines) {
    final s = raw.trim();
    if (s.length >= k) {
      final best = maxSubseq(s, k);
      total += BigInt.parse(best);
    }
  }

  print(total);
}

String maxSubseq(String s, int k) {
  final n = s.length;
  var toRemove = n - k;
  final stack = <int>[];

  for (int i = 0; i < n; i++) {
    final d = s.codeUnitAt(i);
    while (toRemove > 0 && stack.isNotEmpty && stack.last < d) {
      stack.removeLast();
      toRemove--;
    }
    stack.add(d);
  }

  return String.fromCharCodes(stack.take(k));
}
