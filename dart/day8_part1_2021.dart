import 'dart:io';

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  int count = 0;
  for (String line in lines) {
    List<String> parts = line.split(' | ');
    String output = parts[1];
    List<String> digits = output.split(' ');
    for (String digit in digits) {
      switch (digit.length) {
        case 2:
        case 4:
        case 3:
        case 7:
          count++;
          break;
      }
    }
  }

  print(count);
}