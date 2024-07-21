
import 'dart:io';

void main() async {
  final directions = await File('input.txt').readAsString();
  final visitedHouses = <String, bool>{};
  int x = 0, y = 0;

  visitedHouses['$x,$y'] = true;

  for (var dir in directions.runes) {
    switch (String.fromCharCode(dir)) {
      case '^': y++; break;
      case 'v': y--; break;
      case '>': x++; break;
      case '<': x--; break;
    }
    visitedHouses['$x,$y'] = true;
  }

  print(visitedHouses.length);
}
