
import 'dart:io';

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  final parts = input.split('\n\n');
  final patterns = parts[0].split(', ').map((s) => s.trim()).toList();
  final designs = parts[1].trim().split('\n').map((s) => s.trim()).toList();

  int totalPossibleWays = 0;
  for (final design in designs) {
    totalPossibleWays += countWays(design, patterns);
  }
  print(totalPossibleWays);
}


int countWays(String design, List<String> patterns) {
  final memo = <String, int>{};
  
  int countWaysRecursive(String remainingDesign) {
      if (memo.containsKey(remainingDesign)) {
          return memo[remainingDesign]!;
      }

      if (remainingDesign.isEmpty) {
          return 1; 
      }
      
      int ways = 0;
      for (final pattern in patterns) {
        if (remainingDesign.startsWith(pattern)) {
          ways += countWaysRecursive(remainingDesign.substring(pattern.length));
        }
      }
      
      memo[remainingDesign] = ways;
      return ways;
  }

  return countWaysRecursive(design);
}
