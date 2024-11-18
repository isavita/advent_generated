
import 'dart:io';
import 'dart:math';

class Coord {
  double x, y, z;
  Coord(this.x, this.y, this.z);
}

class Point {
  Coord pos;
  Coord vel;
  Point(this.pos, this.vel);
}

class Solution {
  static List<Point> parseInput(List<String> input) {
    return input.map((line) {
      var parts = line.split('@').map((p) => p.trim().split(',').map(double.parse).toList()).toList();
      return Point(
        Coord(parts[0][0], parts[0][1], parts[0][2]),
        Coord(parts[1][0], parts[1][1], parts[1][2]),
      );
    }).toList();
  }

  static bool isIntersecting2D(Point p1, Point p2) {
    final det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y;
    if (det == 0) return false;

    final t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det;
    final t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det;

    final intersectX = p1.pos.x + p1.vel.x * t1;
    final intersectY = p1.pos.y + p1.vel.y * t1;

    return t1 >= 0 && t2 >= 0;
  }

  static int solve(List<String> input, double min, double max) {
    final points = parseInput(input);
    var cnt = 0;

    for (var i = 0; i < points.length; i++) {
      for (var j = 0; j < i; j++) {
        if (isIntersecting2D(points[i], points[j])) {
          final intersectX = points[i].pos.x + points[i].vel.x * 
            ((points[j].vel.y * (points[j].pos.x - points[i].pos.x) - 
              points[j].vel.x * (points[j].pos.y - points[i].pos.y)) / 
            (points[i].vel.x * points[j].vel.y - points[j].vel.x * points[i].vel.y));
          
          final intersectY = points[i].pos.y + points[i].vel.y * 
            ((points[j].vel.y * (points[j].pos.x - points[i].pos.x) - 
              points[j].vel.x * (points[j].pos.y - points[i].pos.y)) / 
            (points[i].vel.x * points[j].vel.y - points[j].vel.x * points[i].vel.y));

          if (min <= intersectX && intersectX <= max && 
              min <= intersectY && intersectY <= max) {
            cnt++;
          }
        }
      }
    }
    return cnt;
  }

  static List<String> readFile(String fileName) {
    return File(fileName).readAsLinesSync();
  }

  static void main() {
    final input = readFile('input.txt');
    print(solve(input, 200000000000000, 400000000000000));
  }
}

void main() {
  Solution.main();
}
