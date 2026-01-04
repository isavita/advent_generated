
import 'dart:io';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final ranges = <_Range>[];
  final ids = <int>[];

  int i = 0;
  for (; i < lines.length && lines[i].trim().isNotEmpty; i++) {
    final p = lines[i].split('-');
    ranges.add(_Range(int.parse(p[0]), int.parse(p[1])));
  }
  for (i++; i < lines.length; i++) {
    ids.addAll(lines[i].split(RegExp(r'\s+')).map(int.parse));
  }

  ranges.sort((a, b) => a.s.compareTo(b.s));
  final merged = <_Range>[];
  for (final r in ranges) {
    if (merged.isNotEmpty && r.s <= merged.last.e + 1) {
      if (r.e > merged.last.e) merged.last.e = r.e;
    } else {
      merged.add(r);
    }
  }

  int fresh = 0;
  for (final id in ids) {
    for (final m in merged) {
      if (id < m.s) break;
      if (id <= m.e) {
        fresh++;
        break;
      }
    }
  }
  print(fresh);
}

class _Range {
  int s, e;
  _Range(this.s, this.e);
}
