
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  int sum = 0;
  
  for (int i = 0; i < lines.length; i += 3) {
    String group1 = lines[i];
    String group2 = lines[i + 1];
    String group3 = lines[i + 2];
    
    Set<String> commonItems = group1.split('').toSet()
      ..retainAll(group2.split('').toSet())
      ..retainAll(group3.split('').toSet());
    
    for (String item in commonItems) {
      int priority = item.codeUnitAt(0) - (item.toLowerCase() == item ? 96 : 38);
      sum += priority;
    }
  }
  
  print(sum);
}
