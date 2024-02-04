
import 'dart:io';
import 'dart:math';

void main() {
  List<List<bool>> asteroids = readAsteroids("input.txt");
  int maxCount = findBestAsteroidLocation(asteroids);
  print(maxCount);
}

List<List<bool>> readAsteroids(String filename) {
  List<List<bool>> asteroids = [];
  File file = File(filename);
  List<String> lines = file.readAsLinesSync();
  for (String line in lines) {
    List<bool> asteroidRow = [];
    for (int i = 0; i < line.length; i++) {
      asteroidRow.add(line[i] == '#');
    }
    asteroids.add(asteroidRow);
  }
  return asteroids;
}

int findBestAsteroidLocation(List<List<bool>> asteroids) {
  int maxCount = 0;
  for (int y = 0; y < asteroids.length; y++) {
    for (int x = 0; x < asteroids[y].length; x++) {
      if (asteroids[y][x]) {
        int count = countVisibleAsteroids(asteroids, x, y);
        if (count > maxCount) {
          maxCount = count;
        }
      }
    }
  }
  return maxCount;
}

int countVisibleAsteroids(List<List<bool>> asteroids, int x, int y) {
  Map<double, bool> angles = {};
  for (int otherY = 0; otherY < asteroids.length; otherY++) {
    for (int otherX = 0; otherX < asteroids[otherY].length; otherX++) {
      if (asteroids[otherY][otherX] && !(otherX == x && otherY == y)) {
        double angle = atan2(otherY - y, otherX - x);
        angles[angle] = true;
      }
    }
  }
  return angles.length;
}
