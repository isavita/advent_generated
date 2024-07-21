
import 'dart:io';
import 'dart:collection';

class Point {
  final int x, y;
  Point(this.x, this.y);
}

class State {
  final Point position;
  final int keys;
  final int steps;

  State(this.position, this.keys, this.steps);
}

void main() async {
  final lines = await File('input.txt').readAsLines();
  final map = lines.map((line) => line.split('')).toList();
  final start = findStart(map);
  final totalKeys = countKeys(map);
  final result = bfs(map, start, totalKeys);
  print(result);
}

Point findStart(List<List<String>> map) {
  for (int y = 0; y < map.length; y++) {
    for (int x = 0; x < map[y].length; x++) {
      if (map[y][x] == '@') {
        return Point(x, y);
      }
    }
  }
  throw Exception('Start point not found');
}

int countKeys(List<List<String>> map) {
  int count = 0;
  for (var row in map) {
    for (var cell in row) {
      if (cell.codeUnitAt(0) >= 'a'.codeUnitAt(0) && cell.codeUnitAt(0) <= 'z'.codeUnitAt(0)) {
        count++;
      }
    }
  }
  return count;
}

int bfs(List<List<String>> map, Point start, int totalKeys) {
  final queue = Queue<State>();
  final visited = Set<String>();
  
  queue.add(State(start, 0, 0));
  visited.add('${start.x},${start.y},0');

  final directions = [Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0)];

  while (queue.isNotEmpty) {
    final current = queue.removeFirst();

    // Check if we have collected all keys
    if (current.keys == (1 << totalKeys) - 1) {
      return current.steps;
    }

    for (var dir in directions) {
      final nextPos = Point(current.position.x + dir.x, current.position.y + dir.y);
      if (isInBounds(map, nextPos) && map[nextPos.y][nextPos.x] != '#') {
        final cell = map[nextPos.y][nextPos.x];
        int newKeys = current.keys;

        if (cell.codeUnitAt(0) >= 'a'.codeUnitAt(0) && cell.codeUnitAt(0) <= 'z'.codeUnitAt(0)) {
          newKeys |= (1 << (cell.codeUnitAt(0) - 'a'.codeUnitAt(0)));
        } else if (cell.codeUnitAt(0) >= 'A'.codeUnitAt(0) && cell.codeUnitAt(0) <= 'Z'.codeUnitAt(0)) {
          if ((current.keys & (1 << (cell.codeUnitAt(0) - 'A'.codeUnitAt(0))) == 0)) {
            continue; // We don't have the key for this door
          }
        }

        final stateKey = '${nextPos.x},${nextPos.y},$newKeys';
        if (!visited.contains(stateKey)) {
          visited.add(stateKey);
          queue.add(State(nextPos, newKeys, current.steps + 1));
        }
      }
    }
  }
  return -1; // If no solution is found
}

bool isInBounds(List<List<String>> map, Point p) {
  return p.y >= 0 && p.y < map.length && p.x >= 0 && p.x < map[p.y].length;
}
