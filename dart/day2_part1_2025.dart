
import 'dart:io';

bool isInvalid(int n) {
  String s = n.toString();
  int len = s.length;
  if (len.isOdd) return false;
  int half = len ~/ 2;
  return s.substring(0, half) == s.substring(half);
}

void main() {
  String raw = File('input.txt').readAsStringSync().trim();
  int total = 0;

  for (String seg in raw.split(',')) {
    List<String> parts = seg.trim().split('-');
    int lo = int.parse(parts[0]);
    int hi = int.parse(parts[1]);

    // Start at the first even-length number in range
    int start = lo;
    while (start.toString().length.isOdd && start <= hi) start++;
    if (start > hi) continue;

    // Iterate only over even-length numbers
    for (int n = start; n <= hi; ) {
      if (isInvalid(n)) total += n;

      int len = n.toString().length;
      int next = n + 1;
      // If incrementing crosses to next length, skip odd-lengths
      if (next.toString().length > len) {
        next = int.parse('1' * (len + 1));
      }
      n = next;
    }
  }

  print(total);
}
