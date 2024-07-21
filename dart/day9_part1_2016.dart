
import 'dart:io';
import 'dart:convert';

void main() async {
  final input = await File('input.txt').readAsString();
  final decompressedLength = getDecompressedLength(input);
  print(decompressedLength);
}

int getDecompressedLength(String input) {
  final regex = RegExp(r'\((\d+)x(\d+)\)');
  int length = 0;
  int i = 0;

  while (i < input.length) {
    final match = regex.matchAsPrefix(input, i);
    if (match != null) {
      final charCount = int.parse(match[1]!);
      final repeatCount = int.parse(match[2]!);
      length += charCount * repeatCount;
      i = match.end + charCount;
    } else {
      length++;
      i++;
    }
  }
  return length;
}
