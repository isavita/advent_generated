import 'dart:io';
import 'dart:convert';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  List<List<String>> ground = List.generate(1, (index) => List.generate(1, (index) => '+'));
  
  int maxX = 0, minX = 0, maxY = 0, minY = 20;
  int xOffset = 500, yOffset = 0;
  
  for (String line in lines) {
    List<String> split = regSplit(line, "[=, .]+");
    if (split[0] == "x") {
      int x = strToInt(split[1]) - xOffset;
      int y1 = strToInt(split[3]) - yOffset;
      int y2 = strToInt(split[4]) - yOffset;
      
      while (x >= maxX) {
        maxX++;
        for (int j = 0; j < ground.length; j++) {
          ground[j].add('.');
        }
      }
      while (x <= minX) {
        minX--;
        for (int j = 0; j < ground.length; j++) {
          ground[j].insert(0, '.');
        }
      }
      while (y2 > maxY) {
        maxY++;
        ground.add(List.generate(ground[0].length, (index) => '.'));
      }
      if (y1 < minY) {
        minY = y1;
      }
      for (int i = y1; i <= y2; i++) {
        ground[i][x - minX] = '#';
      }
    } else {
      int y = strToInt(split[1]) - yOffset;
      int x1 = strToInt(split[3]) - xOffset;
      int x2 = strToInt(split[4]) - xOffset;
      
      while (y > maxY) {
        maxY++;
        ground.add(List.generate(ground[0].length, (index) => '.'));
      }
      while (x2 >= maxX) {
        maxX++;
        for (int j = 0; j < ground.length; j++) {
          ground[j].add('.');
        }
      }
      while (x1 <= minX) {
        minX--;
        for (int j = 0; j < ground.length; j++) {
          ground[j].insert(0, '.');
        }
      }
      for (int i = x1; i <= x2; i++) {
        ground[y][i - minX] = '#';
      }
      if (y < minY) {
        minY = y;
      }
    }
  }

  int waterCount = 0;
  int flowCount = 0;
  int roundLimit = 200000;

  while (ground[1][-minX] != '|' && waterCount < roundLimit) {
    bool canMove = true;
    int x = -minX;
    int y = 1;
    int tryLeft = 0;
    while (canMove) {
      if (y + 1 > maxY || ground[y + 1][x] == '|') {
        ground[y][x] = '|';
        canMove = false;
        if (y >= minY) {
          flowCount++;
        }
      } else if (ground[y + 1][x] == '.') {
        y++;
        tryLeft = 0;
      } else if (ground[y + 1][x] == '#' || ground[y + 1][x] == '~') {
        if ((tryLeft == 1 && ground[y][x - 1] == '|') ||
            (tryLeft == 2 && ground[y][x + 1] == '|') ||
            (ground[y][x + 1] == '|' && ground[y][x - 1] != '.') ||
            (ground[y][x + 1] != '.' && ground[y][x - 1] == '|')) {
          ground[y][x] = '|';
          flowCount++;
          canMove = false;
          for (int i = x + 1; ground[y][i] == '~'; i++) {
            ground[y][i] = '|';
            waterCount--;
            flowCount++;
          }
          for (int i = x - 1; ground[y][i] == '~'; i--) {
            ground[y][i] = '|';
            waterCount--;
            flowCount++;
          }
        } else if ((tryLeft == 0 && ground[y][x - 1] == '.') ||
            (tryLeft == 1 && ground[y][x - 1] == '.')) {
          x--;
          tryLeft = 1;
        } else if ((tryLeft == 0 && ground[y][x + 1] == '.') ||
            (tryLeft == 2 && ground[y][x + 1] == '.')) {
          x++;
          tryLeft = 2;
        } else {
          canMove = false;
          ground[y][x] = '~';
          waterCount++;
        }
      }
    }
  }

  print(flowCount + waterCount);
}

int strToInt(String s) {
  return int.parse(s);
}

List<String> regSplit(String text, String delimiter) {
  RegExp reg = RegExp(delimiter);
  Iterable<Match> matches = reg.allMatches(text);
  List<String> result = [];
  int lastStart = 0;
  for (Match match in matches) {
    result.add(text.substring(lastStart, match.start));
    lastStart = match.end;
  }
  result.add(text.substring(lastStart));
  return result;
}