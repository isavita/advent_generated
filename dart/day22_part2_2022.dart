
import 'dart:io';

class P {
  int x, y;
  P(this.x, this.y);
}

enum Dir { N, E, S, W }

extension DirExtension on Dir {
  Dir rotate(String direction) {
    switch (direction) {
      case 'R':
        return Dir.values[(index + 1) % 4];
      case 'L':
        return Dir.values[(index - 1 + 4) % 4];
      default:
        return this;
    }
  }

  int points() {
    return (index + 3) % 4;
  }
}

class Movement {
  int? steps;
  String? rotate;
  Movement({this.steps, this.rotate});
}

Map<String, bool> map = {};
int size = 0;
List<Movement> movements = [];
List<P> dirs = [
  P(-1, 0),
  P(0, 1),
  P(1, 0),
  P(0, -1),
];

class Human {
  P curr;
  Dir facing;
  Human(this.curr, this.facing);
}

void main() {
  parse();
  Human human = Human(P(0, size), Dir.E);
  for (Movement mov in movements) {
    human.facing = human.facing.rotate(mov.rotate ?? '');
    for (int i = 0; i < (mov.steps ?? 0); i++) {
      if (!human.walk()) {
        break;
      }
    }
  }
  print(1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points());
}

void parse() {
  List<String> lines = File('input.txt').readAsLinesSync();
  for (int r = 0; r < lines.length; r++) {
    String line = lines[r];
    if (line.isEmpty) {
      break;
    }
    if (r == 0) {
      size = line.length ~/ 3;
    }
    for (int c = 0; c < line.length; c++) {
      switch (line[c]) {
        case ' ':
          continue;
        case '#':
          map['$r,$c'] = true;
        case '.':
          map['$r,$c'] = false;
      }
    }
  }
  movements = parsePath(lines.last);
}

List<Movement> parsePath(String path) {
  List<Movement> movements = [];
  int acc = 0;
  for (int i = 0; i < path.length; i++) {
    String char = path[i];
    switch (char) {
      case 'R':
        movements.add(Movement(steps: acc));
        acc = 0;
        movements.add(Movement(rotate: 'R'));
      case 'L':
        movements.add(Movement(steps: acc));
        acc = 0;
        movements.add(Movement(rotate: 'L'));
      default:
        acc = 10 * acc + int.parse(char);
    }
  }
  movements.add(Movement(steps: acc));
  return movements;
}

extension HumanExtension on Human {
  bool walk() {
    P dirDelta = dirs[facing.index];
    P next = P(curr.x + dirDelta.x, curr.y + dirDelta.y);
    if (map.containsKey('${next.x},${next.y}')) {
      if (map['${next.x},${next.y}']!) {
        return false;
      }
      curr = next;
      return true;
    }
    List<dynamic> crossed = crossBorder(next, facing);
    P nextP = crossed[0];
    Dir nextFacing = crossed[1];
    if (map['${nextP.x},${nextP.y}']!) {
      return false;
    }
    curr = nextP;
    facing = nextFacing;
    return true;
  }
}

List<dynamic> crossBorder(P n, Dir dir) {
  int x = n.x, y = n.y;
  switch (true) {
    case true:
      if (x == -1 && y < 2 * size) return [P(y + 2 * size, x + 1), Dir.E];
      if (x == -1 && y >= 2 * size) return [P(x + 4 * size, y - 2 * size), Dir.N];
      if (x == size && dir == Dir.S) return [P(y - size, x + size - 1), Dir.W];
      if (x == 2 * size - 1 && dir == Dir.N) return [P(y + size, x - size + 1), Dir.E];
      if (x == 3 * size && dir == Dir.S) return [P(y + 2 * size, x - 2 * size - 1), Dir.W];
      if (x == 4 * size) return [P(x - 4 * size, y + 2 * size), Dir.S];
      if (y == -1 && x < 3 * size) return [P(3 * size - 1 - x, y + size + 1), Dir.E];
      if (y == -1 && x >= 3 * size) return [P(y + 1, x - 2 * size), Dir.S];
      if (y == size - 1 && x < size) return [P(3 * size - 1 - x, y - size + 1), Dir.E];
      if (y == size - 1 && x >= size && dir == Dir.W) return [P(y + size + 1, x - size), Dir.S];
      if (y == size && dir == Dir.E) return [P(y + 2 * size - 1, x - 2 * size), Dir.N];
      if (y == 2 * size && x < 2 * size && dir == Dir.E) return [P(y - size - 1, x + size), Dir.N];
      if (y == 2 * size && x >= 2 * size) return [P(3 * size - 1 - x, y + size - 1), Dir.W];
      if (y == 3 * size) return [P(3 * size - 1 - x, y - size - 1), Dir.W];
      throw 'not a border crossing';
    default:
      throw 'not a border crossing';
  }
}
