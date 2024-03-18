import 'dart:io';
import 'dart:convert';
import 'dart:core';

class Instruction {
  String left;
  String right;

  Instruction(this.left, this.right);
}

const elemToMatch = 'ZZZ';

void main() async {
  final file = await File('input.txt').readAsString();
  final input = file.trim();

  final lines = input.split('\n');

  final desertMap = <String, Instruction>{};

  for (int i = 2; i < lines.length; i++) {
    if (lines[i].isEmpty) continue;
    final matches = RegExp(r'[A-Z]{3}').allMatches(lines[i]).map((m) => m.group(0)!).toList();
    desertMap[matches[0]] = Instruction(matches[1], matches[2]);
  }

  String current = 'AAA';
  int steps = 0;

  while (current != elemToMatch) {
    for (int i = 0; i < lines[0].length; i++) {
      final direction = lines[0][i];
      if (direction == 'R') {
        current = desertMap[current]!.right;
      } else if (direction == 'L') {
        current = desertMap[current]!.left;
      }
      steps++;

      if (current == elemToMatch) break;
    }
  }

  print(steps);
}