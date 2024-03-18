import 'dart:io';
import 'dart:core';

void main() {
  Map<String, String> rules = {};

  File('input.txt').readAsLines().then((lines) {
    for (String line in lines) {
      List<String> parts = line.split(' => ');
      rules[parts[0]] = parts[1];
    }

    List<String> grid = ['.#.', '..#', '###'];

    for (int i = 0; i < 5; i++) {
      int newSize, subSize;

      if (grid.length % 2 == 0) {
        subSize = 2;
        newSize = grid.length ~/ 2 * 3;
      } else {
        subSize = 3;
        newSize = grid.length ~/ 3 * 4;
      }

      List<String> newGrid = List.generate(newSize, (_) => '');

      for (int y = 0; y < grid.length; y += subSize) {
        for (int x = 0; x < grid.length; x += subSize) {
          List<String> square = [];
          for (int dy = 0; dy < subSize; dy++) {
            square.add(grid[y + dy].substring(x, x + subSize));
          }
          String newSquare = enhance(square.join('/'), rules);
          for (int dy = 0; dy < subSize + 1; dy++) {
            newGrid[y ~/ subSize * (subSize + 1) + dy] +=
                newSquare.split('/')[dy];
          }
        }
      }
      grid = newGrid;
    }

    int count = 0;
    for (String row in grid) {
      for (int i = 0; i < row.length; i++) {
        if (row[i] == '#') {
          count++;
        }
      }
    }
    print(count);
  });
}

String enhance(String input, Map<String, String> rules) {
  for (int i = 0; i < 4; i++) {
    if (rules.containsKey(input)) {
      return rules[input]!;
    }
    input = rotate(input);
  }
  input = flip(input);
  for (int i = 0; i < 4; i++) {
    if (rules.containsKey(input)) {
      return rules[input]!;
    }
    input = rotate(input);
  }
  return '';
}

String rotate(String input) {
  List<String> parts = input.split('/');
  int size = parts.length;
  List<String> newParts = List.generate(size, (_) => '');
  for (int x = 0; x < size; x++) {
    String newRow = '';
    for (int y = size - 1; y >= 0; y--) {
      newRow += parts[y][x];
    }
    newParts[x] = newRow;
  }
  return newParts.join('/');
}

String flip(String input) {
  List<String> parts = input.split('/');
  parts = parts.map((part) => part.split('').reversed.join()).toList();
  return parts.join('/');
}