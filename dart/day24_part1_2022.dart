
import 'dart:io';
import 'dart:collection';

enum Direction { up, down, left, right }

Direction directionFromChar(String char) {
  switch (char) {
    case '^':
      return Direction.up;
    case 'v':
      return Direction.down;
    case '<':
      return Direction.left;
    case '>':
      return Direction.right;
    default:
      throw ArgumentError('Invalid direction character: $char');
  }
}

String charFromDirection(Direction dir) {
  switch (dir) {
    case Direction.up:
      return '^';
    case Direction.down:
      return 'v';
    case Direction.left:
      return '<';
    case Direction.right:
      return '>';
  }
}

void main() {
  final grid = File('input.txt').readAsLinesSync();
  final rows = grid.length;
  final cols = grid[0].length;

  Point start = Point(0, grid[0].indexOf('.'));
  Point end = Point(rows - 1, grid[rows - 1].indexOf('.'));

  final blizzardPositions = <Direction, List<Point>>{};
  blizzardPositions[Direction.up] = [];
  blizzardPositions[Direction.down] = [];
  blizzardPositions[Direction.left] = [];
  blizzardPositions[Direction.right] = [];

  for (int r = 0; r < rows; ++r) {
    for (int c = 0; c < cols; ++c) {
      final char = grid[r][c];
      if (char != '#' && char != '.') {
        blizzardPositions[directionFromChar(char)]!.add(Point(r, c));
      }
    }
  }

  final innerRows = rows - 2;
  final innerCols = cols - 2;
  final cycleLength = innerRows * innerCols ~/ gcd(innerRows, innerCols);

  final blizzardCache = List.generate(cycleLength, (time) {
    final blizzardLocations = <Point>{};
    for (final dir in Direction.values) {
      for (final pos in blizzardPositions[dir]!) {
        int r = pos.row;
        int c = pos.col;
        for (int t = 0; t < time; ++t) {
          switch (dir) {
            case Direction.up:
              r--;
              if (r == 0) r = innerRows;
              break;
            case Direction.down:
              r++;
              if (r == rows - 1) r = 1;
              break;
            case Direction.left:
              c--;
              if (c == 0) c = innerCols;
              break;
            case Direction.right:
              c++;
              if (c == cols - 1) c = 1;
              break;
          }
        }
        int finalR = r;
        int finalC = c;

        if (dir == Direction.up) finalR = (pos.row - 1 - time) % innerRows;
        if (dir == Direction.down) finalR = (pos.row - 1 + time) % innerRows;
        if (dir == Direction.left) finalC = (pos.col - 1 - time) % innerCols;
        if (dir == Direction.right) finalC = (pos.col - 1 + time) % innerCols;

        if (dir == Direction.up || dir == Direction.down) finalR += 1;
        if (dir == Direction.left || dir == Direction.right) finalC += 1;


        int startR = pos.row;
        int startC = pos.col;
        for (int t = 0; t < time; ++t) {
          switch (dir) {
            case Direction.up: startR--; break;
            case Direction.down: startR++; break;
            case Direction.left: startC--; break;
            case Direction.right: startC++; break;
          }
          if (grid[startR][startC] == '#') {
            switch (dir) {
              case Direction.up: startR = rows - 2; break;
              case Direction.down: startR = 1; break;
              case Direction.left: startC = cols - 2; break;
              case Direction.right: startC = 1; break;
            }
          }
        }
        blizzardLocations.add(Point(startR, startC));


      }
    }
    return blizzardLocations;
  });


  int solve() {
    final queue = Queue<State>();
    queue.add(State(start, 0));
    final visited = <PointTime>{};
    visited.add(PointTime(start, 0));

    while (queue.isNotEmpty) {
      final currentState = queue.removeFirst();
      final currentPos = currentState.point;
      final currentTime = currentState.time;

      if (currentPos == end) {
        return currentTime;
      }

      final nextTime = (currentTime + 1) % cycleLength;
      final nextBlizzards = blizzardCache[nextTime];

      final possibleMoves = [
        Point(0, 0), // Wait
        Point(0, 1), // Right
        Point(0, -1), // Left
        Point(1, 0), // Down
        Point(-1, 0), // Up
      ];

      for (final move in possibleMoves) {
        final nextRow = currentPos.row + move.row;
        final nextCol = currentPos.col + move.col;
        final nextPos = Point(nextRow, nextCol);

        if (nextRow >= 0 && nextRow < rows && nextCol >= 0 && nextCol < cols && grid[nextRow][nextCol] != '#') {
          if (!nextBlizzards.contains(nextPos)) {
            final nextState = PointTime(nextPos, currentTime + 1);
            if (!visited.contains(nextState)) {
              visited.add(nextState);
              queue.add(State(nextPos, currentTime + 1));
            }
          }
        }
      }
    }
    return -1; // No path found
  }

  print(solve());
}


int gcd(int a, int b) {
  while (b != 0) {
    final t = b;
    b = a % b;
    a = t;
  }
  return a;
}


class Point {
  final int row;
  final int col;
  Point(this.row, this.col);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point &&
          runtimeType == other.runtimeType &&
          row == other.row &&
          col == other.col;

  @override
  int get hashCode => row.hashCode ^ col.hashCode;

  @override
  String toString() {
    return '($row, $col)';
  }
}

class State {
  final Point point;
  final int time;
  State(this.point, this.time);
}

class PointTime {
  final Point point;
  final int time;
  PointTime(this.point, this.time);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PointTime &&
          runtimeType == other.runtimeType &&
          point == other.point &&
          time == other.time;

  @override
  int get hashCode => point.hashCode ^ time.hashCode;
}
