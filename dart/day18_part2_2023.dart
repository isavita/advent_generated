import 'dart:io';
import 'dart:math';

class Coord {
  int x, y;
  Coord(this.x, this.y);

  Coord add(Coord other) => Coord(x + other.x, y + other.y);
  Coord multiplyByScalar(int s) => Coord(x * s, y * s);
}

final north = Coord(0, -1);
final west = Coord(-1, 0);
final south = Coord(0, 1);
final east = Coord(1, 0);

int abs(int x) => x < 0 ? -x : x;

List<Coord> parseInput(List<String> input) {
  const up = '3', left = '2', down = '1', right = '0';
  Coord current = Coord(0, 0);
  final vertices = [current];

  for (final line in input) {
    final parts = line.split(' ');
    final color = parts[2];
    final dirInput = color[7];
    final lengthStr = color.substring(2, 7);
    final length = hexStringToInt(lengthStr);

    Coord dir;
    switch (dirInput) {
      case up:
        dir = north;
        break;
      case left:
        dir = west;
        break;
      case down:
        dir = south;
        break;
      case right:
        dir = east;
        break;
      default:
        dir = Coord(0, 0); // Assign a default value to 'dir' to avoid the error
    }

    current = current.add(dir.multiplyByScalar(length));
    vertices.add(current);
  }

  return vertices;
}

int hexStringToInt(String hexStr) => int.parse(hexStr, radix: 16);

int shoelace(List<Coord> vertices) {
  final n = vertices.length;
  int area = 0;

  for (int i = 0; i < n; i++) {
    final next = (i + 1) % n;
    area += vertices[i].x * vertices[next].y;
    area -= vertices[i].y * vertices[next].x;
  }

  return (area.abs() ~/ 2);
}

int perimeter(List<Coord> vertices) {
  final n = vertices.length;
  int perim = 0;

  for (int i = 0; i < n; i++) {
    final next = (i + 1) % n;
    perim += abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y);
  }

  return perim;
}

int calculatePolygonArea(List<Coord> vertices) {
  return shoelace(vertices) + perimeter(vertices) ~/ 2 + 1;
}

int solve(List<String> input) {
  final vertices = parseInput(input);
  return calculatePolygonArea(vertices);
}

void main() {
  final input = File('input.txt').readAsLinesSync();
  print(solve(input));
}