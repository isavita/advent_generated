
import 'dart:io';

void main() {
  final lines = File('input.txt').readAsLinesSync();
  final robots = <Robot>[];
  for (final line in lines) {
    robots.add(Robot.parse(line));
  }

  const width = 101;
  const height = 103;
  const seconds = 100;

  for (int i = 0; i < seconds; i++) {
    for (final robot in robots) {
      robot.move(width, height);
    }
  }

  final grid = List.generate(height, (_) => List.filled(width, 0));
  for (final robot in robots) {
    grid[robot.y][robot.x]++;
  }

  final midX = width ~/ 2;
  final midY = height ~/ 2;

  int q1 = 0, q2 = 0, q3 = 0, q4 = 0;
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      if (x < midX && y < midY) {
        q1 += grid[y][x];
      } else if (x > midX && y < midY) {
        q2 += grid[y][x];
      } else if (x < midX && y > midY) {
        q3 += grid[y][x];
      } else if (x > midX && y > midY) {
        q4 += grid[y][x];
      }
    }
  }

  print(q1 * q2 * q3 * q4);
}

class Robot {
  Robot(this.x, this.y, this.vx, this.vy);

  int x;
  int y;
  int vx;
  int vy;

  void move(int width, int height) {
    x += vx;
    y += vy;

    x = (x % width + width) % width;
    y = (y % height + height) % height;
  }

  static Robot parse(String line) {
    final parts = line.split(' ');
    final p = parts[0].substring(2).split(',');
    final v = parts[1].substring(2).split(',');
    return Robot(
      int.parse(p[0]),
      int.parse(p[1]),
      int.parse(v[0]),
      int.parse(v[1]),
    );
  }
}
