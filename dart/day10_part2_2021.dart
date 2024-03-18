import 'dart:io';
import 'dart:collection';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();
  final scores = <int>[];

  for (final line in lines) {
    final score = checkAndCompleteLine(line);
    if (score > 0) {
      scores.add(score);
    }
  }

  scores.sort();
  final middleScore = scores[scores.length ~/ 2];
  print(middleScore);
}

int checkAndCompleteLine(String line) {
  final pairings = {')': '(', ']': '[', '}': '{', '>': '<'};
  final scoreValues = {')': 1, ']': 2, '}': 3, '>': 4};
  final opening = '([{<';
  final closing = ')]}>';
  final stack = Queue<String>();

  for (final char in line.runes) {
    final c = String.fromCharCode(char);
    if (opening.contains(c)) {
      stack.addLast(c);
    } else if (closing.contains(c)) {
      if (stack.isEmpty || stack.last != pairings[c]) {
        return 0; // Corrupted line
      }
      stack.removeLast();
    }
  }

  if (stack.isEmpty) {
    return 0; // Line is not incomplete
  }

  // Calculate score for incomplete line
  int score = 0;
  while (stack.isNotEmpty) {
    final closingChar = getClosingChar(stack.removeLast());
    score = score * 5 + scoreValues[closingChar]!;
  }
  return score;
}

String getClosingChar(String openingChar) {
  switch (openingChar) {
    case '(':
      return ')';
    case '[':
      return ']';
    case '{':
      return '}';
    case '<':
      return '>';
    default:
      return ' ';
  }
}