
import 'dart:io';
import 'dart:math';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  if (lines.isEmpty) {
    print('Grand total: 0');
    return;
  }

  final maxWidth = lines.map((l) => l.length).reduce(max);
  final isSep = List.filled(maxWidth, true);

  for (var x = 0; x < maxWidth; x++) {
    var allSpace = true;
    for (final line in lines) {
      if (x < line.length && !line[x].trim().isEmpty) {
        allSpace = false;
        break;
      }
    }
    isSep[x] = allSpace;
  }

  BigInt grandTotal = BigInt.zero;
  var inBlock = false;
  var startCol = 0;

  for (var x = 0; x < maxWidth; x++) {
    if (!isSep[x]) {
      if (!inBlock) {
        inBlock = true;
        startCol = x;
      }
    } else if (inBlock) {
      grandTotal += processBlock(lines, startCol, x - 1);
      inBlock = false;
    }
  }
  if (inBlock) grandTotal += processBlock(lines, startCol, maxWidth - 1);

  print('Grand total: $grandTotal');
}

BigInt processBlock(List<String> lines, int start, int end) {
  final nums = <BigInt>[];
  var operator = '*';

  for (var c = start; c <= end; c++) {
    final sb = StringBuffer();
    var hasDigits = false;

    for (final line in lines) {
      if (c < line.length) {
        final ch = line[c];
        if (ch.codeUnitAt(0) >= 48 && ch.codeUnitAt(0) <= 57) {
          sb.write(ch);
          hasDigits = true;
        } else if (ch == '+') {
          operator = '+';
        } else if (ch == '*') {
          operator = '*';
        }
      }
    }

    if (hasDigits) nums.add(BigInt.parse(sb.toString()));
  }

  if (nums.isEmpty) return BigInt.zero;

  if (operator == '*') {
    return nums.reduce((a, b) => a * b);
  } else {
    return nums.reduce((a, b) => a + b);
  }
}
