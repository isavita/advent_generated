
import 'dart:io';

class Position {
  int i, j;
  Position(this.i, this.j);
}

Position findPosition(List<String> mat, String ch) {
  for (int i = 0; i < mat.length; i++) {
    for (int j = 0; j < mat[i].length; j++) {
      if (mat[i][j] == ch) {
        return Position(i, j);
      }
    }
  }
  return Position(-1, -1);
}

bool ok(List<String> mat, Position st, String seq) {
  Position curr = Position(st.i, st.j);
  for (int i = 0; i < seq.length; i++) {
    if (mat[curr.i][curr.j] == ' ') {
      return false;
    }
    String ch = seq[i];
    switch (ch) {
      case '^':
        curr.i--;
        break;
      case 'v':
        curr.i++;
        break;
      case '<':
        curr.j--;
        break;
      case '>':
        curr.j++;
        break;
    }
    if (curr.i < 0 || curr.i >= mat.length || curr.j < 0 || curr.j >= mat[0].length) {
      return false;
    }
  }
  return true;
}

String generateMoves(Position position, String objective, List<String> pad) {
  Position objPos = findPosition(pad, objective);
  StringBuffer ret = StringBuffer();
  if (position.j > objPos.j) {
    ret.writeAll(List.filled(position.j - objPos.j, '<'));
  }
  if (position.i > objPos.i) {
    ret.writeAll(List.filled(position.i - objPos.i, '^'));
  }
  if (position.i < objPos.i) {
    ret.writeAll(List.filled(objPos.i - position.i, 'v'));
  }
  if (position.j < objPos.j) {
    ret.writeAll(List.filled(objPos.j - position.j, '>'));
  }
  if (!ok(pad, position, ret.toString())) {
    ret.clear();
    if (position.j < objPos.j) {
      ret.writeAll(List.filled(objPos.j - position.j, '>'));
    }
    if (position.i > objPos.i) {
      ret.writeAll(List.filled(position.i - objPos.i, '^'));
    }
    if (position.i < objPos.i) {
      ret.writeAll(List.filled(objPos.i - position.i, 'v'));
    }
    if (position.j > objPos.j) {
      ret.writeAll(List.filled(position.j - objPos.j, '<'));
    }
  }
  return ret.toString();
}

int solve(String code, int robots, List<String> keyPad, List<String> robotPad, int maxRobots) {
  if (robots <= 0) {
    return code.length;
  }
  int ret = 0;
  int posi = 3, posj = 2;
  if (robots != maxRobots) {
    posi = 0;
  }
  String moves;
  for (int i = 0; i < code.length; i++) {
    String ch = code[i];
    if (robots == maxRobots) {
      moves = generateMoves(Position(posi, posj), ch, keyPad);
      Position pos = findPosition(keyPad, ch);
      posi = pos.i;
      posj = pos.j;
    } else {
      moves = generateMoves(Position(posi, posj), ch, robotPad);
      Position pos = findPosition(robotPad, ch);
      posi = pos.i;
      posj = pos.j;
    }
    ret += solve(moves + 'A', robots - 1, keyPad, robotPad, maxRobots);
  }
  return ret;
}

void main() {
  File file = File('input.txt');
  String content = file.readAsStringSync();
  int maxRobots = 3;
  List<String> keyPad = [
    '789',
    '456',
    '123',
    ' 0A',
  ];
  List<String> robotPad = [
    ' ^A',
    '<v>',
  ];
  int ret = 0;
  List<String> codes = content.trim().split('\n');
  for (String code in codes) {
    code = code.trim();
    if (code.isEmpty) {
      continue;
    }
    int numericPart = 0;
    for (int i = 0; i < code.length; i++) {
      if (code[i].codeUnitAt(0) >= '0'.codeUnitAt(0) && code[i].codeUnitAt(0) <= '9'.codeUnitAt(0)) {
        numericPart = numericPart * 10 + int.parse(code[i]);
      }
    }
    int sv = solve(code, maxRobots, keyPad, robotPad, maxRobots);
    ret += sv * numericPart;
  }
  print(ret);
}
