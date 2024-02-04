
import 'dart:io';

void main() {
  List<int> buffer = [0];
  int currentPosition = 0;
  int steps = int.parse(File('input.txt').readAsStringSync().trim());

  for (int i = 1; i <= 2017; i++) {
    currentPosition = (currentPosition + steps) % buffer.length + 1;
    buffer.insert(currentPosition, i);
  }

  print(buffer[(currentPosition + 1) % buffer.length]);

  int valueAfterZero = -1;
  currentPosition = 0;
  for (int i = 1; i <= 50000000; i++) {
    currentPosition = (currentPosition + steps) % i + 1;
    if (currentPosition == 1) {
      valueAfterZero = i;
    }
  }

  print(valueAfterZero);
}
