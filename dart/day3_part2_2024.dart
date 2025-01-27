
import 'dart:io';

void main() {
  final input = File('input.txt').readAsStringSync();
  final result1 = calculateSum(input, part2: false);
  final result2 = calculateSum(input, part2: true);
  print(result1);
  print(result2);
}

int calculateSum(String input, {required bool part2}) {
  int sum = 0;
  bool enabled = true;
  final mulRegex = RegExp(r'mul\((\d{1,3}),(\d{1,3})\)');

  for (int i = 0; i < input.length; i++) {
    if (part2) {
      if (input.startsWith('do()', i)) {
        enabled = true;
        i += 3; 
      } else if (input.startsWith("don't()", i)) {
        enabled = false;
        i += 6; 
      }
    }

    final match = mulRegex.matchAsPrefix(input, i);
    if (match != null) {
      if (enabled) {
        final num1 = int.parse(match.group(1)!);
        final num2 = int.parse(match.group(2)!);
        sum += num1 * num2;
      }
      i += match.group(0)!.length - 1; 
    }
  }

  return sum;
}
