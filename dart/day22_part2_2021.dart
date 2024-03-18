import 'dart:io';

class Cube {
  bool isOn;
  int x1, x2, y1, y2, z1, z2;

  Cube(this.isOn, this.x1, this.x2, this.y1, this.y2, this.z1, this.z2);

  Cube? getIntersection(Cube other) {
    int x1 = Math.max(this.x1, other.x1);
    int x2 = Math.min(this.x2, other.x2);
    int y1 = Math.max(this.y1, other.y1);
    int y2 = Math.min(this.y2, other.y2);
    int z1 = Math.max(this.z1, other.z1);
    int z2 = Math.min(this.z2, other.z2);

    if (x1 > x2 || y1 > y2 || z1 > z2) {
      return null;
    }

    bool intersectionState;
    if (this.isOn && other.isOn) {
      intersectionState = false;
    } else if (!this.isOn && !other.isOn) {
      intersectionState = true;
    } else {
      intersectionState = other.isOn;
    }

    return Cube(intersectionState, x1, x2, y1, y2, z1, z2);
  }

  int get volume {
    int vol = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1);
    return isOn ? vol : -vol;
  }
}

void main() {
  final file = File('input.txt');
  final input = file.readAsStringSync().trim();
  final result = solve(input);
  print(result);
}

int solve(String input) {
  final cubes = parseInput(input);

  final finalList = <Cube>[];
  for (final c in cubes) {
    final toAdd = <Cube>[];
    for (final finalCube in finalList) {
      final intersection = finalCube.getIntersection(c);
      if (intersection != null) {
        toAdd.add(intersection);
      }
    }

    if (c.isOn) {
      toAdd.add(c);
    }

    finalList.addAll(toAdd);
  }

  int total = 0;
  for (final c in finalList) {
    total += c.volume;
  }

  return total;
}

List<Cube> parseInput(String input) {
  final cubes = <Cube>[];
  for (final line in input.split('\n')) {
    final parts = line.split(' ');
    final isOn = parts[0] == 'on';
    final coords = parts[1].split(',');
    final x = coords[0].split('=')[1].split('..');
    final y = coords[1].split('=')[1].split('..');
    final z = coords[2].split('=')[1].split('..');
    cubes.add(Cube(
      isOn,
      int.parse(x[0]),
      int.parse(x[1]),
      int.parse(y[0]),
      int.parse(y[1]),
      int.parse(z[0]),
      int.parse(z[1]),
    ));
  }
  return cubes;
}

class Math {
  static int max(int a, int b) => a > b ? a : b;
  static int min(int a, int b) => a < b ? a : b;
}