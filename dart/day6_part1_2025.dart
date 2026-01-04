
import 'dart:io';
import 'dart:math';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  if (lines.isEmpty) return;

  final maxWidth = lines.map((l) => l.length).fold(0, max);
  var grandTotal = BigInt.zero;
  var groupStart = -1;

  for (var col = 0; col <= maxWidth; col++) {
    final empty = !lines.any((l) => col < l.length && l[col] != ' ');

    if (!empty && groupStart == -1) {
      groupStart = col;
    } else if (empty && groupStart != -1) {
      grandTotal += calculateProblem(lines, groupStart, col - 1);
      groupStart = -1;
    }
  }

  print(grandTotal);
}

BigInt calculateProblem(List<String> lines, int start, int end) {
  final lastLine = lines.last;
  final op = lastLine.substring(start, min(end + 1, lastLine.length))
              .split('')
              .firstWhere((c) => c == '+' || c == '*', orElse: () => '+');

  BigInt result = BigInt.zero;
  var first = true;

  for (var i = 0; i < lines.length - 1; i++) {
    final numStr = String.fromCharCodes(
      Iterable.generate(
        end - start + 1,
        (j) => (start + j < lines[i].length ? lines[i][start + j] : ' ').codeUnitAt(0),
      ).where((c) => c != ' '.codeUnitAt(0)),
    );
    if (numStr.isEmpty) continue;
    final n = BigInt.tryParse(numStr) ?? BigInt.zero;

    if (first) {
      result = n;
      first = false;
    } else {
      result = op == '*' ? result * n : result + n;
    }
  }

  return result;
}
