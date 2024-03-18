import 'dart:io';
import 'dart:math';

class Sensor {
  Point<int> pos;
  Point<int> beacon;
  int dist;

  Sensor(this.pos, this.beacon) : dist = manhattan(pos, beacon);
}

int manhattan(Point<int> p, Point<int> q) => (p.x - q.x).abs() + (p.y - q.y).abs();

int distress(List<Sensor> sensors, int maxCoord) {
  for (int x = 0; x <= maxCoord; x++) {
    for (int y = 0; y <= maxCoord; y++) {
      Point<int> p = Point<int>(x, y);
      bool detected = false;
      int skip = 0;
      for (var s in sensors) {
        if (manhattan(s.pos, p) <= s.dist) {
          detected = true;
          int dist = s.dist - (s.pos.x - x).abs();
          skip = max(skip, dist + s.pos.y - y);
        }
      }
      if (!detected) {
        return x * 4000000 + y;
      }
      y += skip;
    }
  }
  return -1;
}

void main() {
  List<Sensor> sensors = [];
  String input = File('input.txt').readAsStringSync();
  for (var line in input.split('\n')) {
    RegExp regex = RegExp(r'x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)');
    Match? match = regex.firstMatch(line);
    if (match != null) {
      int sensorX = int.parse(match.group(1)!);
      int sensorY = int.parse(match.group(2)!);
      int beaconX = int.parse(match.group(3)!);
      int beaconY = int.parse(match.group(4)!);
      sensors.add(Sensor(Point<int>(sensorX, sensorY), Point<int>(beaconX, beaconY)));
    }
  }
  print(distress(sensors, 4000000));
}