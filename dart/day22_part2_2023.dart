
import 'dart:io';

class Point {
  int x, y, z;
  Point(this.x, this.y, this.z);

  @override
  String toString() => '($x, $y, $z)';
}

class Brick {
  Point start, end;
  Brick(this.start, this.end);

  List<Point> getPoints() {
    List<Point> points = [];
    for (int x = start.x; x <= end.x; x++) {
      for (int y = start.y; y <= end.y; y++) {
        for (int z = start.z; z <= end.z; z++) {
          points.add(Point(x, y, z));
        }
      }
    }
    return points;
  }

  @override
  String toString() => '$start~$end';
}

void main() {
  List<Brick> bricks = [];
  File('input.txt').readAsLinesSync().forEach((line) {
    List<String> parts = line.split('~');
    List<int> startCoords = parts[0].split(',').map(int.parse).toList();
    List<int> endCoords = parts[1].split(',').map(int.parse).toList();
    bricks.add(Brick(
      Point(startCoords[0], startCoords[1], startCoords[2]),
      Point(endCoords[0], endCoords[1], endCoords[2]),
    ));
  });

  bricks.sort((a, b) => a.start.z.compareTo(b.start.z));
  settleBricks(bricks);

  Map<int, Set<int>> supportedBy = {};
  Map<int, Set<int>> supports = {};

  for (int i = 0; i < bricks.length; i++) {
    supportedBy[i] = {};
    supports[i] = {};
  }

  for (int i = 0; i < bricks.length; i++) {
    for (int j = i + 1; j < bricks.length; j++) {
      if (isSupporting(bricks[i], bricks[j])) {
        supports[i]!.add(j);
        supportedBy[j]!.add(i);
      }
    }
  }

  int safeToDisintegrate = 0;
  for (int i = 0; i < bricks.length; i++) {
    bool canDisintegrate = true;
    for (int j in supports[i]!) {
      if (supportedBy[j]!.length == 1) {
        canDisintegrate = false;
        break;
      }
    }
    if (canDisintegrate) {
      safeToDisintegrate++;
    }
  }

  print('Part 1: $safeToDisintegrate');

  int totalChainReaction = 0;
  for (int i = 0; i < bricks.length; i++) {
    totalChainReaction += countFallenBricks(i, supports, supportedBy);
  }

  print('Part 2: $totalChainReaction');
}

void settleBricks(List<Brick> bricks) {
  Set<String> occupied = {};
  for (Brick brick in bricks) {
    for (Point p in brick.getPoints()) {
      occupied.add('${p.x},${p.y},${p.z}');
    }
  }

  for (Brick brick in bricks) {
    while (brick.start.z > 1) {
      bool canMoveDown = true;
      for (Point p in brick.getPoints()) {
        String below = '${p.x},${p.y},${p.z - 1}';
        if (occupied.contains(below) && !brick.getPoints().any((bp) => '${bp.x},${bp.y},${bp.z}' == below)) {
          canMoveDown = false;
          break;
        }
      }

      if (!canMoveDown) break;

      for (Point p in brick.getPoints()) {
        occupied.remove('${p.x},${p.y},${p.z}');
      }

      brick.start.z--;
      brick.end.z--;

      for (Point p in brick.getPoints()) {
        occupied.add('${p.x},${p.y},${p.z}');
      }
    }
  }
}

bool isSupporting(Brick a, Brick b) {
  for (Point pa in a.getPoints()) {
    for (Point pb in b.getPoints()) {
      if (pa.x == pb.x && pa.y == pb.y && pa.z + 1 == pb.z) {
        return true;
      }
    }
  }
  return false;
}

int countFallenBricks(int disintegrated, Map<int, Set<int>> supports, Map<int, Set<int>> supportedBy) {
  Set<int> fallen = {disintegrated};
  Set<int> q = {disintegrated};

  while (q.isNotEmpty) {
    int current = q.first;
    q.remove(current);

    for (int supported in supports[current]!) {
      if (!fallen.contains(supported) && supportedBy[supported]!.every((s) => fallen.contains(s))) {
        fallen.add(supported);
        q.add(supported);
      }
    }
  }

  return fallen.length - 1;
}
