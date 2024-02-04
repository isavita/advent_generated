import 'dart:io';
import 'dart:math';

class Asteroid {
  int x, y;
  double angle, dist;

  Asteroid(this.x, this.y, this.angle, this.dist);
}

void main() {
  var asteroids = readAsteroids("input.txt");
  var station = findBestAsteroidLocation(asteroids);
  var vaporized = vaporizeAsteroids(asteroids, station);
  
  if (vaporized.length >= 200) {
    var result = vaporized[199].x * 100 + vaporized[199].y;
    print(result);
  } else {
    print("Less than 200 asteroids were vaporized.");
  }
}

List<List<bool>> readAsteroids(String filename) {
  var file = File(filename);
  var lines = file.readAsLinesSync();
  var asteroids = <List<bool>>[];

  for (var line in lines) {
    var asteroidRow = List.filled(line.length, false);
    for (var i = 0; i < line.length; i++) {
      asteroidRow[i] = line[i] == '#';
    }
    asteroids.add(asteroidRow);
  }
  return asteroids;
}

List<Asteroid> vaporizeAsteroids(List<List<bool>> asteroids, List<int> station) {
  var targets = <Asteroid>[];
  for (var y = 0; y < asteroids.length; y++) {
    for (var x = 0; x < asteroids[y].length; x++) {
      if (asteroids[y][x] && !(x == station[0] && y == station[1])) {
        var angle = atan2(y - station[1], x - station[0]);
        var dist = sqrt(pow(x - station[0], 2) + pow(y - station[1], 2));
        if (angle < -pi / 2) {
          angle += 2 * pi;
        }
        targets.add(Asteroid(x, y, angle, dist));
      }
    }
  }

  targets.sort((a, b) {
    if (a.angle == b.angle) {
      return a.dist.compareTo(b.dist);
    }
    return a.angle.compareTo(b.angle);
  });

  var vaporized = <Asteroid>[];
  var lastAngle = -double.infinity;
  for (var i = 0; i < targets.length;) {
    if (targets[i].angle != lastAngle) {
      vaporized.add(targets[i]);
      lastAngle = targets[i].angle;
      targets.removeAt(i);
    } else {
      i++;
    }
  }
  return vaporized;
}

List<int> findBestAsteroidLocation(List<List<bool>> asteroids) {
  var bestLocation = [0, 0];
  var maxCount = 0;

  for (var y = 0; y < asteroids.length; y++) {
    for (var x = 0; x < asteroids[y].length; x++) {
      if (asteroids[y][x]) {
        var count = countVisibleAsteroids(asteroids, x, y);
        if (count > maxCount) {
          maxCount = count;
          bestLocation = [x, y];
        }
      }
    }
  }

  return bestLocation;
}

int countVisibleAsteroids(List<List<bool>> asteroids, int x, int y) {
  var angles = <double, bool>{};
  for (var otherY = 0; otherY < asteroids.length; otherY++) {
    for (var otherX = 0; otherX < asteroids[otherY].length; otherX++) {
      if (asteroids[otherY][otherX] && !(otherX == x && otherY == y)) {
        var angle = atan2(otherY - y, otherX - x);
        angles[angle] = true;
      }
    }
  }
  return angles.length;
}