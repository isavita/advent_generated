
import 'dart:io';

void main() {
  File input = new File('input.txt');
  List<String> lines = input.readAsLinesSync();

  int result = 0;
  for (String line in lines) {
    result += int.parse(line);
  }

  print(result);
}
