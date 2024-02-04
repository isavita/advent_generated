import 'dart:io';

void main() {
  List<int> jumps = File('input.txt').readAsLinesSync().map(int.parse).toList();
  
  int steps = 0;
  int index = 0;
  
  while (index >= 0 && index < jumps.length) {
    int offset = jumps[index];
    jumps[index] += offset >= 3 ? -1 : 1;
    index += offset;
    steps++;
  }
  
  print(steps);
}