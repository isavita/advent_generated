
import 'dart:io';
import 'dart:collection';

void main() {
  final input = File('input.txt').readAsLinesSync();
  final grid = input;

  final rows = grid.length;
  final cols = grid[0].length;

  Point start = Point(0, grid[0].indexOf('.'));
  Point end = Point(rows - 1, grid[rows - 1].indexOf('.'));

  final blizzards = <Blizzard>[];
  for (int r = 0; r < rows; ++r) {
    for (int c = 0; c < cols; ++c) {
      final char = grid[r][c];
      if (char != '#' && char != '.') {
        blizzards.add(Blizzard(Point(r, c), char));
      }
    }
  }

  Set<Point> getBlizzardPositionsAtTime(int time, int rows, int cols, List<Blizzard> blizzards) {
    final blizzardPositions = <Point>{};
    for (final blizzard in blizzards) {
      int r = blizzard.pos.r;
      int c = blizzard.pos.c;
      switch (blizzard.dir) {
        case '^':
          r = (r - 1 - time) % (rows - 2);
          if (r < 0) r += (rows - 2);
          r += 1;
          break;
        case 'v':
          r = (r - 1 + time) % (rows - 2);
          r += 1;
          break;
        case '<':
          c = (c - 1 - time) % (cols - 2);
          if (c < 0) c += (cols - 2);
          c += 1;
          break;
        case '>':
          c = (c - 1 + time) % (cols - 2);
          c += 1;
          break;
      }
      blizzardPositions.add(Point(r, c));
    }
    return blizzardPositions;
  }

  int solve(Point start, Point end, int startTime, int rows, int cols, List<Blizzard> blizzards) {
    final queue = Queue<State>();
    queue.add(State(start, startTime));
    final visited = <PointTime>{};
    visited.add(PointTime(start, startTime % (lcm(rows - 2, cols - 2))));

    while (queue.isNotEmpty) {
      final currentState = queue.removeFirst();
      final currentPos = currentState.pos;
      final currentTime = currentState.time;

      if (currentPos == end) {
        return currentTime;
      }

      final nextTime = currentTime + 1;
      final nextBlizzardPositions = getBlizzardPositionsAtTime(nextTime, rows, cols, blizzards);

      final moves = [
        Point(0, 0), // Wait
        Point(0, 1), // Right
        Point(0, -1), // Left
        Point(1, 0), // Down
        Point(-1, 0), // Up
      ];

      for (final move in moves) {
        final nextPos = Point(currentPos.r + move.r, currentPos.c + move.c);

        if (nextPos.r >= 0 && nextPos.r < rows && nextPos.c >= 0 && nextPos.c < cols &&
            grid[nextPos.r][nextPos.c] != '#' && !nextBlizzardPositions.contains(nextPos)) {
          final nextStateKey = PointTime(nextPos, nextTime % (lcm(rows - 2, cols - 2)));
          if (!visited.contains(nextStateKey)) {
            visited.add(nextStateKey);
            queue.add(State(nextPos, nextTime));
          }
        }
      }
    }
    return -1; // No path found
  }

  int part1 = solve(start, end, 0, rows, cols, blizzards);
  print('Part 1: $part1');

  int toEnd = part1;
  int toStart = solve(end, start, toEnd, rows, cols, blizzards);
  int toEndAgain = solve(start, end, toStart, rows, cols, blizzards);

  print('Part 2: ${toEnd + (toStart - toEnd) + (toEndAgain - toStart)}');
}

int gcd(int a, int b) {
  while (b != 0) {
    final t = b;
    b = a % b;
    a = t;
  }
  return a;
}

int lcm(int a, int b) => (a * b) ~/ gcd(a, b);

class Point {
  final int r;
  final int c;
  Point(this.r, this.c);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point && runtimeType == other.runtimeType && r == other.r && c == other.c;

  @override
  int get hashCode => r.hashCode ^ c.hashCode;

  @override
  String toString() {
    return '($r, $c)';
  }
}

class Blizzard {
  final Point pos;
  final String dir;
  Blizzard(this.pos, this.dir);
}

class State {
  final Point pos;
  final int time;
  State(this.pos, this.time);
}

class PointTime {
  final Point pos;
  final int time;
  PointTime(this.pos, this.time);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PointTime && runtimeType == other.runtimeType && pos == other.pos && time == other.time;

  @override
  int get hashCode => pos.hashCode ^ time.hashCode;
}
