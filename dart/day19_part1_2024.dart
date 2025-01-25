
import 'dart:io';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final patterns = lines.first.split(',').map((s) => s.trim()).toSet();
  final designs = lines.skip(2).toList();

  int possibleDesigns = 0;
  for (final design in designs) {
    if (isPossible(design, patterns)) {
      possibleDesigns++;
    }
  }

  print(possibleDesigns);
}

bool isPossible(String design, Set<String> patterns) {
  final dp = List<bool>.filled(design.length + 1, false);
  dp[0] = true;

  for (int i = 0; i < design.length; i++) {
    if (!dp[i]) continue;
    for (final pattern in patterns) {
      if (i + pattern.length <= design.length &&
          design.substring(i, i + pattern.length) == pattern) {
        dp[i + pattern.length] = true;
      }
    }
  }

  return dp[design.length];
}
