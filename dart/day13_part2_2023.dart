import 'dart:io';
import 'dart:math';

class Mirror {
  List<int> rows;
  List<int> cols;

  Mirror(this.rows, this.cols);
}

List<Mirror> parseInput(List<String> input) {
  List<Mirror> mirrors = [];
  List<String> mirrorStr = [];

  for (var line in input) {
    if (line.isEmpty) {
      mirrors.add(parseMirror(mirrorStr));
      mirrorStr = [];
    } else {
      mirrorStr.add(line);
    }
  }
  mirrors.add(parseMirror(mirrorStr));

  return mirrors;
}

Mirror parseMirror(List<String> mirrorStr) {
  List<int> rows = List.generate(mirrorStr.length, (_) => 0);
  List<int> cols = List.generate(mirrorStr.first.length, (_) => 0);

  for (int y = 0; y < mirrorStr.length; y++) {
    for (int x = 0; x < mirrorStr[y].length; x++) {
      rows[y] <<= 1;
      cols[x] <<= 1;
      if (mirrorStr[y][x] == '#') {
        rows[y]++;
        cols[x]++;
      }
    }
  }

  return Mirror(rows, cols);
}

int getMirrorAxis(List<int> lines) {
  for (int i = 1; i < lines.length; i++) {
    bool isMirror = true;

    for (int j = 0; isMirror && j < min(i, lines.length - i); j++) {
      if (lines[i - 1 - j] != lines[i + j]) {
        isMirror = false;
      }
    }

    if (isMirror) {
      return i;
    }
  }

  return 0;
}

int getMirrorAxisWithOneSmudge(List<int> lines) {
  for (int i = 1; i < lines.length; i++) {
    bool isMirror = true;
    int numSmudges = 0;

    for (int j = 0; isMirror && j < min(i, lines.length - i); j++) {
      if (lines[i - 1 - j] != lines[i + j]) {
        if (numSmudges > 0) {
          isMirror = false;
        } else {
          int dif = lines[i - 1 - j] ^ lines[i + j];
          bool isOnlyOneSmudge = (dif & (dif - 1)) == 0;
          if (isOnlyOneSmudge) {
            numSmudges++;
          } else {
            isMirror = false;
          }
        }
      }
    }

    if (isMirror && numSmudges == 1) {
      return i;
    }
  }

  return 0;
}

int solve(List<String> input) {
  List<Mirror> mirrors = parseInput(input);

  int res = 0;
  for (var mirror in mirrors) {
    res += getMirrorAxisWithOneSmudge(mirror.cols);
    res += getMirrorAxisWithOneSmudge(mirror.rows) * 100;
  }
  return res;
}

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  print(solve(input));
}