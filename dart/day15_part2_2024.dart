
import 'dart:io';

class Point {
  final int x, y;
  const Point(this.x, this.y);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point &&
          runtimeType == other.runtimeType &&
          x == other.x &&
          y == other.y;

  @override
  int get hashCode => x.hashCode ^ y.hashCode;

  Point operator +(Point other) => Point(x + other.x, y + other.y);
}

const Point up = Point(0, -1);
const Point down = Point(0, 1);
const Point left = Point(-1, 0);
const Point right = Point(1, 0);

void main() {
  final input = File('input.txt').readAsStringSync();
  print(solve(input).toInt());
  print(solve(scaleUp(input)).toInt());
}

double solve(String input) {
  final parsed = parse(input);
  final m = parsed.$1;
  final steps = parsed.$2;

  Point robot = Point(0,0);
  for (final entry in m.entries) {
    if (entry.value == '@') {
      robot = entry.key;
      break;
    }
  }

  for (final dir in steps) {
    if (tryToStep(m, robot, dir)) {
      robot = robot + dir;
    }
  }

  double sum = 0;
  for (final entry in m.entries) {
    if (entry.value == '[' || entry.value == 'O') {
      sum += entry.key.x + 100 * entry.key.y;
    }
  }
  return sum;
}

bool tryToStep(Map<Point, String> m, Point pos, Point dir) {
  final orig = Map<Point, String>.from(m);
  if (m[pos] == '.') {
    return true;
  } else if (m[pos] == 'O' || m[pos] == '@') {
    if (tryToStep(m, pos + dir, dir)) {
      m[pos + dir] = m[pos]!;
      m[pos] = '.';
      return true;
    }
  } else if (m[pos] == ']') {
    if (tryToStep(m, pos + left, dir)) {
      return true;
    }
  } else if (m[pos] == '[') {
    if (dir == left) {
      if (tryToStep(m, pos + left, dir)) {
        m[pos + left] = '[';
        m[pos] = ']';
        m[pos + right] = '.';
        return true;
      }
    } else if (dir == right) {
      if (tryToStep(m, pos + right + right, dir)) {
        m[pos] = '.';
        m[pos + right] = '[';
        m[pos + right + right] = ']';
        return true;
      }
    } else {
      if (tryToStep(m, pos + dir, dir) && tryToStep(m, pos + right + dir, dir)) {
        m[pos] = '.';
        m[pos + right] = '.';
        m[pos + dir] = '[';
        m[pos + dir + right] = ']';
        return true;
      }
    }
  }
  m.clear();
  m.addAll(orig);
  return false;
}

String scaleUp(String input) {
  String s = input;
  s = s.replaceAll('#', '##');
  s = s.replaceAll('.', '..');
  s = s.replaceAll('O', '[]');
  s = s.replaceAll('@', '@.');
  return s;
}

(Map<Point, String>, List<Point>) parse(String input) {
  final blocks = input.trim().split('\n\n');
  final lines = blocks[0].split('\n');
  final m = <Point, String>{};
  for (int y = 0; y < lines.length; y++) {
    for (int x = 0; x < lines[y].length; x++) {
      m[Point(x, y)] = lines[y][x];
    }
  }
  final stepsChars = blocks[1].replaceAll('\n', '').split('');
  final steps = <Point>[];
  for (final ch in stepsChars) {
    switch (ch) {
      case '^':
        steps.add(up);
        break;
      case '<':
        steps.add(left);
        break;
      case '>':
        steps.add(right);
        break;
      case 'v':
        steps.add(down);
        break;
    }
  }
  return (m, steps);
}
