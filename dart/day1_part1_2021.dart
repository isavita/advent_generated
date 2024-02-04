import 'dart:io';

void main() {
  List<int> depths = [];
  File file = new File('input.txt');
  List<String> lines = file.readAsLinesSync();
  
  for (String line in lines) {
    depths.add(int.parse(line));
  }
  
  int count = 0;
  
  for (int i = 1; i < depths.length; i++) {
    if (depths[i] > depths[i - 1]) {
      count++;
    }
  }
  
  print(count);
}