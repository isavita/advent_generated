
import 'dart:io';
import 'dart:convert';

void main() async {
  final input = await File('input.txt').readAsString();
  final ans = solve(input.trim());
  print(ans);
}

int solve(String input) {
  final lines = parseInput(input);
  return lines.map(doMaths).reduce((a, b) => a + b);
}

List<List<String>> parseInput(String input) {
  return input.split('\n').map((line) => line.replaceAll(' ', '').split('')).toList();
}

int doMaths(List<String> input) {
  final stack = <String>[];
  final openIndices = <int>[];

  for (var i = 0; i < input.length; i++) {
    stack.add(input[i]);
    if (input[i] == '(') {
      openIndices.add(stack.length - 1);
    } else if (input[i] == ')') {
      final openIndex = openIndices.removeLast();
      final slice = stack.sublist(openIndex + 1, stack.length - 1);
      stack[openIndex] = calcFlatSlicePart(slice);
      stack.removeRange(openIndex + 1, stack.length);
    }
  }
  return int.parse(calcFlatSlicePart(stack));
}

String calcFlatSlicePart(List<String> input) {
  for (var i = 1; i < input.length - 1; i++) {
    if (input[i] == '+') {
      input[i - 1] = (int.parse(input[i - 1]) + int.parse(input[i + 1])).toString();
      input.removeRange(i, i + 2);
      i--;
    }
  }
  for (var i = 1; i < input.length - 1; i++) {
    if (input[i] == '*') {
      input[i - 1] = (int.parse(input[i - 1]) * int.parse(input[i + 1])).toString();
      input.removeRange(i, i + 2);
      i--;
    }
  }
  return input[0];
}
