
import 'dart:io';

void main() {
  final inputFile = File('input.txt');
  int validTriangleCount = 0;

  try {
    final lines = inputFile.readAsLinesSync();
    for (final line in lines) {
      final sides = line.trim().split(' ').where((s) => s.isNotEmpty).map(int.parse).toList();
      if (sides.length == 3) {
        final a = sides[0];
        final b = sides[1];
        final c = sides[2];

        if (isValidTriangle(a, b, c)) {
          validTriangleCount++;
        }
      }
    }
    print(validTriangleCount);
  } catch (e) {
    print('Error reading input file: $e');
  }
}

bool isValidTriangle(int a, int b, int c) {
  return (a + b > c) && (a + c > b) && (b + c > a);
}
