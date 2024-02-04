
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  int niceCount = 0;
  
  for (String line in lines) {
    if (hasThreeVowels(line) && hasDoubleLetter(line) && !containsForbiddenStrings(line)) {
      niceCount++;
    }
  }
  
  print(niceCount);
}

bool hasThreeVowels(String line) {
  int count = 0;
  for (int i = 0; i < line.length; i++) {
    if ('aeiou'.contains(line[i])) {
      count++;
    }
  }
  return count >= 3;
}

bool hasDoubleLetter(String line) {
  for (int i = 0; i < line.length - 1; i++) {
    if (line[i] == line[i + 1]) {
      return true;
    }
  }
  return false;
}

bool containsForbiddenStrings(String line) {
  return line.contains('ab') || line.contains('cd') || line.contains('pq') || line.contains('xy');
}
