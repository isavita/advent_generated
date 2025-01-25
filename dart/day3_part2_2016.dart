
import 'dart:io';

bool isValidTriangle(List<int> sides) {
  sides.sort();
  return sides[0] + sides[1] > sides[2];
}

void main() {
  // Part 1
  int part1ValidTriangles = 0;
  File('input.txt').readAsLinesSync().forEach((line) {
    List<int> sides = line.trim().split(RegExp(r'\s+')).map(int.parse).toList();
    if (isValidTriangle(sides)) {
      part1ValidTriangles++;
    }
  });
  print(part1ValidTriangles);

  // Part 2
  int part2ValidTriangles = 0;
  List<String> lines = File('input.txt').readAsLinesSync();
  if (lines.isNotEmpty) {
    int numRows = lines.length;
    int numCols = lines[0].trim().split(RegExp(r'\s+')).length;

    for (int j = 0; j < numCols; ++j) {
      for (int i = 0; i < numRows; i += 3) {
        if (i + 2 < numRows) {
          List<int> sides = [];
          sides.add(int.parse(lines[i].trim().split(RegExp(r'\s+'))[j]));
          sides.add(int.parse(lines[i + 1].trim().split(RegExp(r'\s+'))[j]));
          sides.add(int.parse(lines[i + 2].trim().split(RegExp(r'\s+'))[j]));
          if (isValidTriangle(sides)) {
            part2ValidTriangles++;
          }
        }
      }
    }
  }
  print(part2ValidTriangles);
}
