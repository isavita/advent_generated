
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int codeLength = 0;
  int memoryLength = 0;

  for (String line in lines) {
    codeLength += line.length;
    memoryLength += line.substring(1, line.length - 1)
        .replaceAll(RegExp(r'\\x[0-9a-fA-F]{2}|\\["\\]'), 'X')
        .length;
  }

  print(codeLength - memoryLength);
}
