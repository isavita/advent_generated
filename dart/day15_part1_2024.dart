
import 'dart:io';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  List<String> grid = [];
  String moves = '';
  bool readingMap = true;

  for (var line in lines) {
    if (readingMap) {
      if (line.contains('#')) {
        grid.add(line);
      } else {
        readingMap = false;
        moves += line;
      }
    } else {
      moves += line;
    }
  }

  List<List<String>> runes = grid.map((row) => row.split('')).toList();

  int robotR = 0, robotC = 0;
  for (int r = 0; r < runes.length; r++) {
    for (int c = 0; c < runes[r].length; c++) {
      if (runes[r][c] == '@') {
        robotR = r;
        robotC = c;
      }
    }
  }

  Map<String, List<int>> dirs = {
    '^': [-1, 0],
    'v': [1, 0],
    '<': [0, -1],
    '>': [0, 1],
  };

  for (var move in moves.split('')) {
    List<int> d = dirs[move]!;
    int nr = robotR + d[0], nc = robotC + d[1];
    if (runes[nr][nc] == '#') {
      continue;
    } else if (runes[nr][nc] == 'O') {
      if (!pushBoxes(runes, nr, nc, d[0], d[1])) {
        continue;
      }
    }
    if (runes[nr][nc] == '.' || runes[nr][nc] == 'O') {
      runes[robotR][robotC] = '.';
      runes[nr][nc] = '@';
      robotR = nr;
      robotC = nc;
    }
  }

  int sum = 0;
  for (int r = 0; r < runes.length; r++) {
    for (int c = 0; c < runes[r].length; c++) {
      if (runes[r][c] == 'O') {
        sum += r * 100 + c;
      }
    }
  }

  print(sum);
}

bool pushBoxes(List<List<String>> runes, int r, int c, int dr, int dc) {
  int nr = r + dr, nc = c + dc;
  if (runes[nr][nc] == '#') {
    return false;
  }
  if (runes[nr][nc] == 'O') {
    if (!pushBoxes(runes, nr, nc, dr, dc)) {
      return false;
    }
  }
  if (runes[nr][nc] == '.') {
    runes[nr][nc] = 'O';
    runes[r][c] = '.';
    return true;
  }
  return false;
}
