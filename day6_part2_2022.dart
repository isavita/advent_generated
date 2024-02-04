
import 'dart:io';

void main() {
  String input = File('input.txt').readAsStringSync().trim();

  int indexPacket = findStartMarker(input, 4);
  int indexMessage = findStartMarker(input, 14);

  print(indexPacket);
  print(indexMessage);
}

int findStartMarker(String input, int markerLength) {
  for (int i = markerLength - 1; i < input.length; i++) {
    String sub = input.substring(i - markerLength + 1, i + 1);
    if (sub.runes.toSet().length == markerLength) {
      return i + 1;
    }
  }
  return -1;
}
