import 'dart:io';
import 'dart:math';

class Mirror {
  List<int> rows = [];
  List<int> cols = [];
}

List<Mirror> parseInput(List<String> input) {
  List<Mirror> mirrors = [];
  List<String> mirrorStr = [];
  for (var line in input) {
    if (line.isEmpty) {
      mirrors.add(parseMirror(mirrorStr));
      mirrorStr.clear();
    } else {
      mirrorStr.add(line);
    }
  }
  mirrors.add(parseMirror(mirrorStr));
  return mirrors;
}

Mirror parseMirror(List<String> mirrorStr) {
  Mirror mirror = Mirror();
  mirror.rows = List.generate(mirrorStr.length, (_) => 0);
  mirror.cols = List.generate(mirrorStr.first.length, (_) => 0);

  for (var y = 0; y < mirrorStr.length; y++) {
    for (var x = 0; x < mirrorStr[y].length; x++) {
      mirror.rows[y] <<= 1;
      mirror.cols[x] <<= 1;
      if (mirrorStr[y][x] == '#') {
        mirror.rows[y]++;
        mirror.cols[x]++;
      }
    }
  }

  return mirror;
}

int getMirrorAxis(List<int> lines) {
  for (var i = 1; i < lines.length; i++) {
    var isMirror = true;
    for (var j = 0; isMirror && j < min(i, lines.length - i); j++) {
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
  for (var i = 1; i < lines.length; i++) {
    var isMirror = true;
    var numSmudges = 0;
    for (var j = 0; isMirror && j < min(i, lines.length - i); j++) {
      if (lines[i - 1 - j] != lines[i + j]) {
        if (numSmudges > 0) {
          isMirror = false;
        } else {
          var dif = lines[i - 1 - j] ^ lines[i + j];
          var isOnlyOneSmudge = (dif & (dif - 1)) == 0;
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
  var mirrors = parseInput(input);
  var res = 0;
  for (var mirror in mirrors) {
    res += getMirrorAxis(mirror.cols);
    res += getMirrorAxis(mirror.rows) * 100;
  }
  return res;
}

void main() {
  var input = File('input.txt').readAsLinesSync();
  print(solve(input));
}