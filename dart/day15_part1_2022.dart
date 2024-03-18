import 'dart:io';
import 'dart:math';

class Sensor {
  Point<int> pos;
  Point<int> beacon;
  int dist;

  Sensor(this.pos, this.beacon) : dist = manhattan(pos, beacon);
}

int impossible(List<Sensor> sensors, int y) {
  Set<int> pts = {};
  for (var s in sensors) {
    int dist = s.dist - (s.pos.y - y).abs();
    if (dist >= 0) {
      for (int x = 0; x <= dist; x++) {
        pts.add(s.pos.x + x);
        pts.add(s.pos.x - x);
      }
    }
  }
  for (var s in sensors) {
    if (s.beacon.y == y) {
      pts.remove(s.beacon.x);
    }
  }
  return pts.length;
}

int manhattan(Point<int> p, Point<int> q) {
  return (p.x - q.x).abs() + (p.y - q.y).abs();
}

void main() {
  List<Sensor> sensors = [];
  String input = File('input.txt').readAsStringSync();
  for (var line in input.split('\n')) {
    List<int> values = line
        .replaceAll('Sensor at x=', '')
        .replaceAll(', y=', ' ')
        .replaceAll(': closest beacon is at x=', ' ')
        .split(' ')
        .map(int.parse)
        .toList();
    sensors.add(Sensor(Point<int>(values[0], values[1]), Point<int>(values[2], values[3])));
  }
  print(impossible(sensors, 2000000));
}