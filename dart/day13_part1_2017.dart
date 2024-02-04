
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  int severity = 0;
  
  for (String line in lines) {
    List<String> parts = line.split(': ');
    int depth = int.parse(parts[0]);
    int range = int.parse(parts[1]);
    
    if (depth % ((range - 1) * 2) == 0) {
      severity += depth * range;
    }
  }
  
  print(severity);
}
