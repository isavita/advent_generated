
import 'dart:io';
import 'dart:math';

class Star {
  int x, y, vX, vY;
  Star(this.x, this.y, this.vX, this.vY);
}

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<Star> stars = [];
  
  final regex = RegExp(r'position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>');
  
  for (var line in lines) {
    final match = regex.firstMatch(line);
    if (match != null) {
      stars.add(Star(
        int.parse(match.group(1)!),
        int.parse(match.group(2)!),
        int.parse(match.group(3)!),
        int.parse(match.group(4)!)
      ));
    }
  }

  int smallestT = 0;
  int smallestArea = 2147483647;

  for (int t = 1; t < 100000; t++) {
    int maxX = -2147483648;
    int maxY = -2147483648;
    int minX = 2147483647;
    int minY = 2147483647;

    for (var star in stars) {
      int x = star.x + star.vX * t;
      int y = star.y + star.vY * t;
      
      maxX = max(maxX, x);
      minX = min(minX, x);
      maxY = max(maxY, y);
      minY = min(minY, y);
    }

    int area = (maxX - minX) + (maxY - minY);
    
    if (area < smallestArea) {
      smallestArea = area;
      smallestT = t;
    }
  }

  int t = smallestT;
  
  List<List<bool>> mapper = List.generate(
    (stars.map((s) => s.y + s.vY * t).reduce(max) - 
     stars.map((s) => s.y + s.vY * t).reduce(min) + 1), 
    (_) => List.filled(
      (stars.map((s) => s.x + s.vX * t).reduce(max) - 
       stars.map((s) => s.x + s.vX * t).reduce(min) + 1), 
      false
    )
  );

  for (var star in stars) {
    int x = star.x + star.vX * t - stars.map((s) => s.x + s.vX * t).reduce(min);
    int y = star.y + star.vY * t - stars.map((s) => s.y + s.vY * t).reduce(min);
    mapper[y][x] = true;
  }

  for (var row in mapper) {
    print(row.map((cell) => cell ? '#' : ' ').join());
  }
}
