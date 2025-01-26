
import 'dart:io';
import 'dart:math';

class Point {
  int x, y, z;
  Point(this.x, this.y, this.z);

  Point operator +(Point other) => Point(x + other.x, y + other.y, z + other.z);
  Point operator -(Point other) => Point(x - other.x, y - other.y, z - other.z);
  Point operator -() => Point(-x, -y, -z);

  @override
  bool operator ==(Object other) =>
      other is Point && x == other.x && y == other.y && z == other.z;

  @override
  int get hashCode => Object.hash(x, y, z);

  @override
  String toString() => '($x, $y, $z)';
}

class Scanner {
  List<Point> beacons = [];
  Point position = Point(0, 0, 0);
  bool aligned = false;
  Scanner();
}

List<Scanner> parseInput(String input) {
  List<Scanner> scanners = [];
  List<String> lines = input.trim().split('\n');
  Scanner currentScanner = Scanner();
  for (String line in lines) {
    if (line.startsWith('--- scanner')) {
      if (currentScanner.beacons.isNotEmpty) {
        scanners.add(currentScanner);
      }
      currentScanner = Scanner();
    } else if (line.isNotEmpty) {
      List<int> coords = line.split(',').map(int.parse).toList();
      currentScanner.beacons.add(Point(coords[0], coords[1], coords[2]));
    }
  }
  if (currentScanner.beacons.isNotEmpty) {
    scanners.add(currentScanner);
  }
  return scanners;
}

Point rotate(Point p, int rotation) {
  switch (rotation) {
    case 0:  return Point(p.x, p.y, p.z); 
    case 1:  return Point(p.x, -p.z, p.y); 
    case 2:  return Point(p.x, -p.y, -p.z);
    case 3:  return Point(p.x, p.z, -p.y);
    case 4:  return Point(-p.x, p.z, p.y);
    case 5:  return Point(-p.x, -p.y, p.z);
    case 6:  return Point(-p.x, -p.z, -p.y);
    case 7:  return Point(-p.x, p.y, -p.z);
    case 8:  return Point(p.y, p.x, -p.z);
    case 9:  return Point(p.y, p.z, p.x);
    case 10: return Point(p.y, -p.x, p.z);
    case 11: return Point(p.y, -p.z, -p.x);
    case 12: return Point(-p.y, -p.x, -p.z);
    case 13: return Point(-p.y, -p.z, p.x);
    case 14: return Point(-p.y, p.x, p.z);
    case 15: return Point(-p.y, p.z, -p.x);
    case 16: return Point(p.z, p.y, -p.x);
    case 17: return Point(p.z, p.x, p.y);
    case 18: return Point(p.z, -p.y, p.x);
    case 19: return Point(p.z, -p.x, -p.y);
    case 20: return Point(-p.z, -p.y, -p.x);
    case 21: return Point(-p.z, -p.x, p.y);
    case 22: return Point(-p.z, p.y, p.x);
    case 23: return Point(-p.z, p.x, -p.y);
    default: throw Exception('Invalid rotation');
  }
}

List<Point> transform(List<Point> points, Point offset, int rotation) {
  return points.map((p) => rotate(p, rotation) + offset).toList();
}

(Point?, int?) findOverlap(Scanner s1, Scanner s2) {
  for (int r = 0; r < 24; r++) {
    for (Point p1 in s1.beacons) {
      for (Point p2 in s2.beacons) {
        Point offset = p1 - rotate(p2, r);
        List<Point> transformedBeacons = transform(s2.beacons, offset, r);
        int overlapCount = transformedBeacons.where((p) => s1.beacons.contains(p)).length;
          if(overlapCount >= 12) {
              return (offset, r);
          }
      }
    }
  }
  return (null, null);
}

void main() {
  String input = File('input.txt').readAsStringSync();
  List<Scanner> scanners = parseInput(input);

  scanners[0].aligned = true;
  List<Scanner> alignedScanners = [scanners[0]];
  List<Scanner> unalignedScanners = scanners.sublist(1);

  while(unalignedScanners.isNotEmpty) {
      bool found = false;
      for(Scanner s1 in alignedScanners) {
          for(int i = 0; i< unalignedScanners.length; i++){
              Scanner s2 = unalignedScanners[i];
                var (offset, rotation) = findOverlap(s1, s2);
                if(offset != null && rotation != null){
                    s2.position = offset;
                    s2.aligned = true;
                    s2.beacons = transform(s2.beacons, offset, rotation);
                    alignedScanners.add(s2);
                    unalignedScanners.removeAt(i);
                    found = true;
                    break;
                }
          }
          if(found){
              break;
          }
      }
      if(!found) {
          throw Exception("No overlap found, cannot align all scanners");
      }
  }

  Set<Point> allBeacons = {};
  for(Scanner s in alignedScanners) {
    allBeacons.addAll(s.beacons);
  }

  print('Number of beacons: ${allBeacons.length}');

  int maxManhattan = 0;
  for(int i=0; i<alignedScanners.length; i++){
      for(int j = i + 1; j<alignedScanners.length; j++){
         var diff = alignedScanners[i].position - alignedScanners[j].position;
         int dist = diff.x.abs() + diff.y.abs() + diff.z.abs();
         maxManhattan = max(maxManhattan, dist);
      }
  }

   print('Max Manhattan Distance: $maxManhattan');
}
