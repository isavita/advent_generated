
import 'dart:io';
import 'dart:math';

class Point3D {
  final int x, y, z;
  const Point3D(this.x, this.y, this.z);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Point3D &&
          runtimeType == other.runtimeType &&
          x == other.x &&
          y == other.y &&
          z == other.z;

  @override
  int get hashCode => x.hashCode ^ y.hashCode ^ z.hashCode;
}

class Scanner {
  int number;
  int x, y, z;
  List<List<int>> relativeCoords;
  late List<List<List<int>>> rotations;
  List<Point3D> absoluteCoords = [];
  Set<Point3D> absoluteCoordsMap = {};

  Scanner(this.number, this.relativeCoords)
      : x = 0,
        y = 0,
        z = 0 {
    fillRotations();
  }

  void fillAbsoluteCoordsMap() {
    absoluteCoordsMap = absoluteCoords.map((p) => p).toSet();
  }

  void fillRotations() {
    rotations = [];
    List<List<List<int>>> sixRotations = [];

    sixRotations.add(relativeCoords);

    List<List<int>> dir2 = [];
    List<List<int>> dir3 = [];
    List<List<int>> dir4 = [];
    List<List<int>> dir5 = [];
    List<List<int>> dir6 = [];

    for (final c in relativeCoords) {
      int x = c[0], y = c[1], z = c[2];
      dir2.add([x, -y, -z]);
      dir3.add([x, -z, y]);
      dir4.add([-y, -z, x]);
      dir5.add([-x, -z, -y]);
      dir6.add([y, -z, -x]);
    }
    sixRotations.addAll([dir2, dir3, dir4, dir5, dir6]);

    for (final rotation in sixRotations) {
      rotations.add(rotation);
      List<List<int>> r2 = [];
      List<List<int>> r3 = [];
      List<List<int>> r4 = [];
      for (final c in rotation) {
        int x = c[0], y = c[1], z = c[2];
        r2.add([-y, x, z]);
        r3.add([-x, -y, z]);
        r4.add([y, -x, z]);
      }
      rotations.addAll([r2, r3, r4]);
    }
  }
}

FindResult? findAbsoluteCoordsForScanner(
    Scanner undet, List<Scanner> settled) {
  for (final rotatedCoords in undet.rotations) {
    for (final setScanner in settled) {
      for (final absCoord in setScanner.absoluteCoords) {
        for (final relativeCoord in rotatedCoords) {
          List<Point3D> unsettledAbsoluteCoords =
              makeAbsoluteCoordsList(absCoord, relativeCoord, rotatedCoords);

          int matchingCount = 0;
          for (final ac in unsettledAbsoluteCoords) {
            if (setScanner.absoluteCoordsMap.contains(ac)) {
              matchingCount++;
            }
          }

          if (matchingCount >= 12) {
            undet.relativeCoords = rotatedCoords;
            undet.absoluteCoords = unsettledAbsoluteCoords;
            undet.fillAbsoluteCoordsMap();
            undet.x = absCoord.x - relativeCoord[0];
            undet.y = absCoord.y - relativeCoord[1];
            undet.z = absCoord.z - relativeCoord[2];
            return FindResult(scanner: undet, didUpdate: true);
          }
        }
      }
    }
  }
  return FindResult(scanner: undet, didUpdate: false);
}

class FindResult {
  Scanner scanner;
  bool didUpdate;
  FindResult({required this.scanner, required this.didUpdate});
}

List<Point3D> makeAbsoluteCoordsList(
    Point3D absolute, List<int> relative, List<List<int>> relativeCoords) {
  Point3D diff = Point3D(
    absolute.x - relative[0],
    absolute.y - relative[1],
    absolute.z - relative[2],
  );

  List<Point3D> absCoords = [];
  for (final c in relativeCoords) {
    absCoords.add(Point3D(
      diff.x + c[0],
      diff.y + c[1],
      diff.z + c[2],
    ));
  }
  return absCoords;
}

List<Scanner> parseInput(String input) {
  List<Scanner> scanners = [];
  List<String> rawScanners = input.split("\n\n");
  for (final rawScanner in rawScanners) {
    List<String> lines = rawScanner.split("\n");
    int number = int.parse(lines[0].split(" ")[2]);

    List<List<int>> coords = [];
    for (int i = 1; i < lines.length; i++) {
      List<String> parts = lines[i].split(",");
      coords.add([
        int.parse(parts[0]),
        int.parse(parts[1]),
        int.parse(parts[2]),
      ]);
    }
    scanners.add(Scanner(number, coords));
  }
  return scanners;
}

int solve(String input) {
  List<Scanner> scanners = parseInput(input);

  List<Scanner> settled = [scanners[0]];
  settled[0].absoluteCoords =
      scanners[0].relativeCoords.map((c) => Point3D(c[0], c[1], c[2])).toList();
  settled[0].fillAbsoluteCoordsMap();

  List<Scanner> undetermined = scanners.sublist(1);

  while (undetermined.isNotEmpty) {
    bool updated = false;
    for (int i = 0; i < undetermined.length; i++) {
      FindResult? result = findAbsoluteCoordsForScanner(undetermined[i], settled);
      if (result != null && result.didUpdate) {
        settled.add(result.scanner);
        undetermined.removeAt(i);
        updated = true;
        break;
      }
    }
    if (!updated && undetermined.isNotEmpty) {
      break; // No scanner could be settled in this iteration, avoid infinite loop for unsolvable inputs
    }
  }

  int furthest = 0;
  for (int i = 0; i < settled.length; i++) {
    for (int j = i + 1; j < settled.length; j++) {
      int manhattanDist = (settled[i].x - settled[j].x).abs() +
          (settled[i].y - settled[j].y).abs() +
          (settled[i].z - settled[j].z).abs();
      if (manhattanDist > furthest) {
        furthest = manhattanDist;
      }
    }
  }
  return furthest;
}

void main() {
  File inputFile = File('input.txt');
  String input = inputFile.readAsStringSync().trim();
  int result = solve(input);
  print(result);
}
