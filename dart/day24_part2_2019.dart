import 'dart:io';

const int Side = 5;
const int Square = Side * Side;

List<bool> parse() {
  final List<bool> res = List.filled(Square, false);
  final file = File('input.txt');
  final lines = file.readAsLinesSync();
  for (int row = 0; row < Side; row++) {
    for (int col = 0; col < Side; col++) {
      res[row * Side + col] = lines[row][col] == '#';
    }
  }
  return res;
}

Map<int, List<bool>> next2(Map<int, List<bool>> space) {
  final newSpace = <int, List<bool>>{};
  final minMax = minMaxLevel(space);
  final minLevel = minMax[0] - 1;
  final maxLevel = minMax[1] + 1;

  for (int level = minLevel; level <= maxLevel; level++) {
    newSpace[level] = List.filled(Square, false);
    for (int cell = 0; cell < Square; cell++) {
      if (cell == 12) continue;
      final row = cell ~/ Side;
      final col = cell % Side;
      int neighbours = 0;

      if (row == 0) {
        if (infested(space, level - 1, 7)) neighbours++;
      }
      if (col == 0) {
        if (infested(space, level - 1, 11)) neighbours++;
      }
      if (col == 4) {
        if (infested(space, level - 1, 13)) neighbours++;
      }
      if (row == 4) {
        if (infested(space, level - 1, 17)) neighbours++;
      }

      if (cell == 7) {
        for (int i = 0; i < Side; i++) {
          if (infested(space, level + 1, i)) neighbours++;
        }
      }
      if (cell == 11) {
        for (int i = 0; i < Side; i++) {
          if (infested(space, level + 1, 5 * i)) neighbours++;
        }
      }
      if (cell == 13) {
        for (int i = 0; i < Side; i++) {
          if (infested(space, level + 1, 5 * i + Side - 1)) neighbours++;
        }
      }
      if (cell == 17) {
        for (int i = 0; i < Side; i++) {
          if (infested(space, level + 1, (Side - 1) * Side + i)) neighbours++;
        }
      }

      if (row > 0 && cell != 17) {
        if (infested(space, level, cell - Side)) neighbours++;
      }
      if (col > 0 && cell != 13) {
        if (infested(space, level, cell - 1)) neighbours++;
      }
      if (col < Side - 1 && cell != 11) {
        if (infested(space, level, cell + 1)) neighbours++;
      }
      if (row < Side - 1 && cell != 7) {
        if (infested(space, level, cell + Side)) neighbours++;
      }

      if (infested(space, level, cell) && neighbours != 1) {
        newSpace[level]![cell] = false;
      } else if (!infested(space, level, cell) && (neighbours == 1 || neighbours == 2)) {
        newSpace[level]![cell] = true;
      } else {
        newSpace[level]![cell] = infested(space, level, cell);
      }
    }
  }

  clean(newSpace);
  return newSpace;
}

void clean(Map<int, List<bool>> space) {
  final minMax = minMaxLevel(space);
  final min = minMax[0];
  final max = minMax[1];

  int countMin = 0, countMax = 0;
  for (int cell = 0; cell < Square; cell++) {
    if (space[min]![cell]) countMin++;
    if (space[max]![cell]) countMax++;
  }
  if (countMin == 0) space.remove(min);
  if (countMax == 0) space.remove(max);
}

bool infested(Map<int, List<bool>> space, int level, int cell) {
  return space[level]?[cell] ?? false;
}

List<int> minMaxLevel(Map<int, List<bool>> space) {
  int min = 999999, max = -999999;
  for (final level in space.keys) {
    if (level < min) min = level;
    if (level > max) max = level;
  }
  return [min, max];
}

void main() {
  final input = parse();
  var space = {0: input};

  for (int i = 0; i < 200; i++) {
    space = next2(space);
  }

  int count = 0;
  for (final grid in space.values) {
    for (final cell in grid) {
      if (cell) count++;
    }
  }
  print(count);
}