
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  int count = 0;
  
  for (String line in lines) {
    List<List<int>> ranges = [];
    List<String> parts = line.split(',');
    
    for (String part in parts) {
      List<int> range = part.split('-').map(int.parse).toList();
      ranges.add(range);
    }
    
    if ((ranges[0][0] <= ranges[1][0] && ranges[0][1] >= ranges[1][1]) ||
        (ranges[1][0] <= ranges[0][0] && ranges[1][1] >= ranges[0][1])) {
      count++;
    }
  }
  
  print(count);
}
