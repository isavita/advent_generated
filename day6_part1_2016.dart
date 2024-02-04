
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  for (int i = 0; i < lines[0].length; i++) {
    Map<String, int> charCount = {};
    for (String line in lines) {
      String char = line[i];
      charCount[char] = (charCount[char] ?? 0) + 1;
    }
    
    String mostCommonChar = charCount.entries.reduce((a, b) => a.value > b.value ? a : b).key;
    stdout.write(mostCommonChar);
  }
}
