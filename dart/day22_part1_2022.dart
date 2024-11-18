
import 'dart:io';
import 'dart:convert';

class P {
  int x, y;
  P(this.x, this.y);
  
  @override
  bool operator ==(Object other) {
    return other is P && x == other.x && y == other.y;
  }
  
  @override
  int get hashCode => x.hashCode ^ y.hashCode;
}

enum Dir { N, E, S, W }

extension DirExt on Dir {
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
  int steps;
  String? rotate;
  Movement({this.steps = 0, this.rotate});
}

class Human {
  P curr;
  Dir facing;
  Human({required this.curr, this.facing = Dir.E});
}

void main() {
  final input = File('input.txt').readAsLinesSync();
  
  final map = <P, bool>{};
  late int size;
  final movements = <Movement>[];
  
  // Parse map
  size = input[0].length ~/ 3;
  for (int r = 0; r < input.length; r++) {
    final line = input[r];
    if (line.isEmpty) break;
    
    for (int c = 0; c < line.length; c++) {
      switch (line[c]) {
        case '#':
          map[P(r, c)] = true;
          break;
        case '.':
          map[P(r, c)] = false;
          break;
      }
    }
  }
  
  // Parse movements
  movements.addAll(parsePath(input.last));
  
  // Directions
  final dirs = [
    P(-1, 0),  // N
    P(0, 1),   // E
    P(1, 0),   // S
    P(0, -1)   // W
  ];
  
  // Initial human position
  var human = Human(curr: P(0, size), facing: Dir.E);
  
  // Move
  for (var mov in movements) {
    if (mov.rotate != null) {
      human.facing = human.facing.rotate(mov.rotate!);
    }
    
    for (int i = 0; i < mov.steps; i++) {
      if (!walk(human, map, dirs, size)) break;
    }
  }
  
  // Calculate final score
  print(1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points());
}

List<Movement> parsePath(String path) {
  final movements = <Movement>[];
  int acc = 0;
  
  for (var char in path.split('')) {
    if (['R', 'L'].contains(char)) {
      if (acc > 0) movements.add(Movement(steps: acc));
      acc = 0;
      movements.add(Movement(rotate: char));
    } else {
      acc = 10 * acc + int.parse(char);
    }
  }
  
  if (acc > 0) movements.add(Movement(steps: acc));
  return movements;
}

bool walk(Human human, Map<P, bool> map, List<P> dirs, int size) {
  final dir = dirs[human.facing.index];
  
  var next = P(human.curr.x + dir.x, human.curr.y + dir.y);
  
  if (map.containsKey(next)) {
    if (map[next]!) return false;
    human.curr = next;
    return true;
  }
  
  // Fast-forward in opposite direction
  final oppDir = P(-dir.x, -dir.y);
  while (true) {
    final lookAhead = P(next.x + oppDir.x, next.y + oppDir.y);
    
    if (!map.containsKey(lookAhead)) {
      if (map[next]!) return false;
      human.curr = next;
      return true;
    }
    
    next = lookAhead;
  }
}
