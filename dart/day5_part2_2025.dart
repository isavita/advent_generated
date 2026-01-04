
import 'dart:io';

class Range {
  final int start;
  final int end;
  Range(this.start, this.end);
}

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final rawRanges = <Range>[];
  final availableIds = <int>[];
  var parsingRanges = true;

  for (final line in lines.map((l) => l.trim())) {
    if (line.isEmpty) {
      if (rawRanges.isNotEmpty) parsingRanges = false;
      continue;
    }
    if (parsingRanges && line.contains('-')) {
      final parts = line.split('-');
      rawRanges.add(Range(int.parse(parts[0]), int.parse(parts[1])));
    } else {
      parsingRanges = false;
      availableIds.add(int.parse(line));
    }
  }

  if (rawRanges.isEmpty) return;

  rawRanges.sort((a, b) => a.start.compareTo(b.start));
  final merged = <Range>[rawRanges.first];

  for (final r in rawRanges.skip(1)) {
    final last = merged.last;
    if (r.start <= last.end) {
      if (r.end > last.end) merged[merged.length - 1] = Range(last.start, r.end);
    } else {
      merged.add(r);
    }
  }

  int freshAvailableCount = 0;
  for (final id in availableIds) {
    for (final r in merged) {
      if (id >= r.start && id <= r.end) {
        freshAvailableCount++;
        break;
      }
    }
  }

  final totalFreshIds = merged.fold<int>(0, (sum, r) => sum + (r.end - r.start + 1));

  print('Part One: $freshAvailableCount');
  print('Part Two: $totalFreshIds');
}
