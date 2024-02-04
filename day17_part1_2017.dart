
import 'dart:io';

void main() {
  List<int> buffer = [0];
  int steps = int.parse(File('input.txt').readAsStringSync().trim());
  int currentPosition = 0;

  for (int i = 1; i <= 2017; i++) {
    currentPosition = (currentPosition + steps) % buffer.length + 1;
    buffer.insert(currentPosition, i);
  }

  print(buffer[(currentPosition + 1) % buffer.length]);
}
