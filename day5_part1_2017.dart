import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<int> offsets = lines.map(int.parse).toList();

  int index = 0;
  int steps = 0;

  while (index >= 0 && index < offsets.length) {
    int jump = offsets[index];
    offsets[index]++;
    index += jump;
    steps++;
  }

  print(steps);
}