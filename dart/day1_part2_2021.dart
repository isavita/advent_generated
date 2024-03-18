import 'dart:io';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();
  final vals = lines.map(int.parse).toList();

  int prevSum = vals[0] + vals[1] + vals[2];
  int count = 0;
  for (int i = 3; i < vals.length; i++) {
    int currSum = vals[i - 2] + vals[i - 1] + vals[i];
    if (currSum > prevSum) {
      count++;
    }
    prevSum = currSum;
  }

  print(count);
}