
import 'dart:io';

class Robot {
  int x, y, vx, vy;
  Robot(this.x, this.y, this.vx, this.vy);
}

int mod(int a, int b) {
  return (a % b + b) % b;
}

Robot parseLine(String line) {
  final regex = RegExp(r'p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)');
  final match = regex.firstMatch(line);
  if (match == null || match.groupCount != 4) {
    throw FormatException('Invalid line format: $line');
  }
  return Robot(
    int.parse(match.group(1)!),
    int.parse(match.group(2)!),
    int.parse(match.group(3)!),
    int.parse(match.group(4)!),
  );
}

void moveRobots(List<Robot> robots, int sizeX, int sizeY) {
  for (final robot in robots) {
    robot.x = mod(robot.x + robot.vx, sizeX);
    robot.y = mod(robot.y + robot.vy, sizeY);
  }
}

List<int> countQuadrants(List<Robot> robots, int sizeX, int sizeY) {
  final counts = [0, 0, 0, 0];
  final centerX = sizeX / 2;
  final centerY = sizeY / 2;

  for (final robot in robots) {
    final x = robot.x;
    final y = robot.y;
    if (x < centerX) {
      if (y < centerY) {
        counts[0]++;
      } else if (y > centerY) {
        counts[1]++;
      }
    } else if (x > centerX) {
      if (y < centerY) {
        counts[2]++;
      } else if (y > centerY) {
        counts[3]++;
      }
    }
  }
  return counts;
}

bool hasNoOverlaps(List<Robot> robots) {
  final positions = <String>{};
  for (final robot in robots) {
    final pos = '${robot.x},${robot.y}';
    if (positions.contains(pos)) {
      return false;
    }
    positions.add(pos);
  }
  return true;
}

void drawGrid(List<Robot> robots, int sizeX, int sizeY) {
  final gridMap = <String>{};
  for (final robot in robots) {
    gridMap.add('${robot.x},${robot.y}');
  }

  for (var y = 0; y < sizeY; y++) {
    var line = '';
    for (var x = 0; x < sizeX; x++) {
      if (gridMap.contains('$x,$y')) {
        line += '#';
      } else {
        line += '.';
      }
    }
    print(line);
  }
}

void main() {
  final sizeX = 101;
  final sizeY = 103;

  final file = File('input.txt');
  List<Robot> robots = [];

  try {
    robots = file.readAsLinesSync().where((line) => line.isNotEmpty).map(parseLine).toList();
  } catch (e) {
    print('Error reading input.txt: $e');
    exit(1);
  }

  // Part 1
  final robotsPart1 = robots.map((r) => Robot(r.x, r.y, r.vx, r.vy)).toList();

  for (var n = 0; n < 100; n++) {
    moveRobots(robotsPart1, sizeX, sizeY);
  }

  final counts = countQuadrants(robotsPart1, sizeX, sizeY);
  final safetyFactor = counts.reduce((a, b) => a * b);
  print('Part 1 - Safety Factor after 100 seconds: $safetyFactor');

  // Part 2
  final robotsPart2 = robots.map((r) => Robot(r.x, r.y, r.vx, r.vy)).toList();

  var seconds = 0;
  while (true) {
    if (hasNoOverlaps(robotsPart2)) {
      break;
    }
    moveRobots(robotsPart2, sizeX, sizeY);
    seconds++;
    if (seconds > 200000) {
      print('Exceeded maximum iterations without finding a unique position configuration.');
      exit(1);
    }
  }
  print('Part 2 - Fewest seconds to display Easter egg: $seconds');
  print('Final positions of robots:');
  drawGrid(robotsPart2, sizeX, sizeY);
}
