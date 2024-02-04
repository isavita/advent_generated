
import 'dart:io';

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  List<String> area = List.from(input);
  int minutes = 10;

  for (int i = 0; i < minutes; i++) {
    List<String> nextArea = List.from(area);

    for (int y = 0; y < area.length; y++) {
      for (int x = 0; x < area[y].length; x++) {
        int trees = 0;
        int lumberyards = 0;

        for (int dy = -1; dy <= 1; dy++) {
          for (int dx = -1; dx <= 1; dx++) {
            if (dy == 0 && dx == 0) continue;
            if (y + dy >= 0 && y + dy < area.length && x + dx >= 0 && x + dx < area[y].length) {
              if (area[y + dy][x + dx] == '|') trees++;
              if (area[y + dy][x + dx] == '#') lumberyards++;
            }
          }
        }

        if (area[y][x] == '.' && trees >= 3) {
          nextArea[y] = nextArea[y].replaceRange(x, x + 1, '|');
        } else if (area[y][x] == '|' && lumberyards >= 3) {
          nextArea[y] = nextArea[y].replaceRange(x, x + 1, '#');
        } else if (area[y][x] == '#') {
          if (trees == 0 || lumberyards == 0) {
            nextArea[y] = nextArea[y].replaceRange(x, x + 1, '.');
          }
        }
      }
    }

    area = List.from(nextArea);
  }

  int woodedAcres = 0;
  int lumberyardAcres = 0;

  for (String row in area) {
    woodedAcres += row.split('').where((element) => element == '|').length;
    lumberyardAcres += row.split('').where((element) => element == '#').length;
  }

  print(woodedAcres * lumberyardAcres);
}
