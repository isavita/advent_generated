import 'dart:io';

void main() {
  final file = File('input.txt');
  final totalScore = file.readAsLinesSync().fold(0, (score, line) {
    final result = checkLine(line);
    return result.corrupted ? score + result.score : score;
  });
  print(totalScore);
}

Result checkLine(String line) {
  final pairings = {')': '(', ']': '[', '}': '{', '>': '<'};
  final scores = {')': 3, ']': 57, '}': 1197, '>': 25137};
  final stack = <String>[];

  for (final char in line.runes.map((rune) => String.fromCharCode(rune))) {
    switch (char) {
      case '(':
      case '[':
      case '{':
      case '<':
        stack.add(char);
        break;
      case ')':
      case ']':
      case '}':
      case '>':
        if (stack.isEmpty || stack.last != pairings[char]) {
          return Result(scores[char]!, true);
        }
        stack.removeLast();
        break;
    }
  }

  return const Result(0, false);
}

class Result {
  final int score;
  final bool corrupted;

  const Result(this.score, this.corrupted);
}