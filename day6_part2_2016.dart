
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  String part1 = '';
  String part2 = '';
  
  for (int i = 0; i < lines[0].length; i++) {
    Map<String, int> charCount = {};
    
    for (int j = 0; j < lines.length; j++) {
      String char = lines[j][i];
      charCount[char] = (charCount[char] ?? 0) + 1;
    }
    
    part1 += charCount.keys.firstWhere((key) => charCount[key] == charCount.values.reduce((a, b) => a > b ? a : b));
    part2 += charCount.keys.firstWhere((key) => charCount[key] == charCount.values.reduce((a, b) => a < b ? a : b));
  }
  
  print(part1);
  print(part2);
}
