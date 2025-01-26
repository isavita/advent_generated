
import 'dart:io';

class Position {
  int i, j;
  Position(this.i, this.j);
}

class CacheKey {
  Position pos;
  String ch;
  String padType;
  CacheKey(this.pos, this.ch, this.padType);

  @override
  int get hashCode => pos.hashCode ^ ch.hashCode ^ padType.hashCode;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is CacheKey &&
          runtimeType == other.runtimeType &&
          pos == other.pos &&
          ch == other.ch &&
          padType == other.padType;
}

class SolveKey {
  String code;
  int robots;
  int maxRobots;
  SolveKey(this.code, this.robots, this.maxRobots);

  @override
  int get hashCode => code.hashCode ^ robots.hashCode ^ maxRobots.hashCode;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is SolveKey &&
          runtimeType == other.runtimeType &&
          code == other.code &&
          robots == other.robots &&
          maxRobots == other.maxRobots;
}

var positionCache = <String, Position>{};
var okCache = <String, bool>{};
var moveCache = <CacheKey, String>{};
var solveCache = <SolveKey, int>{};

Position findPosition(List<String> mat, String ch) {
  String key = ch + mat.join('');
  if (positionCache.containsKey(key)) {
    return positionCache[key]!;
  }

  for (int i = 0; i < mat.length; i++) {
    for (int j = 0; j < mat[i].length; j++) {
      if (mat[i][j] == ch) {
        Position pos = Position(i, j);
        positionCache[key] = pos;
        return pos;
      }
    }
  }
  return Position(-1, -1);
}

bool ok(List<String> mat, Position st, String seq) {
  String key = '${st.i},${st.j},$seq,${mat.join("")}';
  if (okCache.containsKey(key)) {
    return okCache[key]!;
  }

  Position curr = Position(st.i, st.j);
  for (int i = 0; i < seq.length; i++) {
    if (mat[curr.i][curr.j] == ' ') {
      okCache[key] = false;
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

    if (curr.i < 0 ||
        curr.i >= mat.length ||
        curr.j < 0 ||
        curr.j >= mat[0].length) {
      okCache[key] = false;
      return false;
    }
  }

  okCache[key] = true;
  return true;
}

String generateMoves(Position position, String objective, List<String> pad) {
  CacheKey key = CacheKey(position, objective, pad.join(""));
  if (moveCache.containsKey(key)) {
    return moveCache[key]!;
  }

  Position objPos = findPosition(pad, objective);

  String result = '';
  if (position.j > objPos.j) {
    result += '<' * (position.j - objPos.j);
  }
  if (position.i > objPos.i) {
    result += '^' * (position.i - objPos.i);
  }
  if (position.i < objPos.i) {
    result += 'v' * (objPos.i - position.i);
  }
  if (position.j < objPos.j) {
    result += '>' * (objPos.j - position.j);
  }

  if (!ok(pad, position, result)) {
    result = '';
    if (position.j < objPos.j) {
      result += '>' * (objPos.j - position.j);
    }
    if (position.i > objPos.i) {
      result += '^' * (position.i - objPos.i);
    }
    if (position.i < objPos.i) {
      result += 'v' * (objPos.i - position.i);
    }
    if (position.j > objPos.j) {
      result += '<' * (position.j - objPos.j);
    }
  }

  moveCache[key] = result;
  return result;
}

int solve(String code, int robots, List<String> keyPad, List<String> robotPad,
    int maxRobots) {
  SolveKey key = SolveKey(code, robots, maxRobots);
  if (solveCache.containsKey(key)) {
    return solveCache[key]!;
  }

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
    ret += solve(moves + "A", robots - 1, keyPad, robotPad, maxRobots);
  }

  solveCache[key] = ret;
  return ret;
}

void main() {
  String content = File('input.txt').readAsStringSync();

  int maxRobots = 26;
  List<String> keyPad = [
    "789",
    "456",
    "123",
    " 0A",
  ];
  List<String> robotPad = [
    " ^A",
    "<v>",
  ];

  int ret = 0;
  List<String> codes = content.trim().split('\n');

  for (String code in codes) {
    code = code.trim();
    if (code == "") {
      continue;
    }

    int numericPart = 0;
    for (int i = 0; i < code.length; i++) {
      if (code[i].codeUnitAt(0) >= '0'.codeUnitAt(0) &&
          code[i].codeUnitAt(0) <= '9'.codeUnitAt(0)) {
        numericPart = numericPart * 10 + (code[i].codeUnitAt(0) - '0'.codeUnitAt(0));
      }
    }

    int sv = solve(code, maxRobots, keyPad, robotPad, maxRobots);
    ret += sv * numericPart;
  }

  print(ret);
}
