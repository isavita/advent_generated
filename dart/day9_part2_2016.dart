
import 'dart:io';

void main() {
  String input = File('input.txt').readAsStringSync().replaceAll(RegExp(r'\s+'), '');

  int part1 = decompressedLength(input, false);
  int part2 = decompressedLength(input, true);

  print(part1);
  print(part2);
}

int decompressedLength(String input, bool recursive) {
  int length = 0;
  int i = 0;

  while (i < input.length) {
    if (input[i] == '(') {
      int markerEnd = input.indexOf(')', i);
      String marker = input.substring(i + 1, markerEnd);
      List<String> parts = marker.split('x');
      int repeatLength = int.parse(parts[0]);
      int repeatCount = int.parse(parts[1]);

      i = markerEnd + 1;
      String repeated = input.substring(i, i + repeatLength);

      if (recursive) {
        length += repeatCount * decompressedLength(repeated, true);
      } else {
        length += repeatCount * repeatLength;
      }

      i += repeatLength;
    } else {
      length++;
      i++;
    }
  }

  return length;
}
