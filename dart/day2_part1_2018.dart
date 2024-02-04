
import 'dart:io';

void main() {
  List<String> boxIds = File('input.txt').readAsLinesSync();
  
  int twoCount = 0;
  int threeCount = 0;
  
  for (String id in boxIds) {
    Map<String, int> charCount = {};
    
    for (int i = 0; i < id.length; i++) {
      charCount.update(id[i], (value) => value + 1, ifAbsent: () => 1);
    }
    
    if (charCount.containsValue(2)) {
      twoCount++;
    }
    
    if (charCount.containsValue(3)) {
      threeCount++;
    }
  }
  
  int checksum = twoCount * threeCount;
  print(checksum);
}
