
import 'dart:io';
import 'dart:math';

class Pt3 {
  int x, y, z;
  Pt3(this.x, this.y, this.z);

  Pt3 add(Pt3 other) => Pt3(x + other.x, y + other.y, z + other.z);

  @override
  bool operator ==(Object other) =>
      other is Pt3 && x == other.x && y == other.y && z == other.z;

  @override
  int get hashCode => Object.hash(x, y, z);
}

void main() {
  Set<Pt3> cubes = {};
  List<Pt3> neighbors = [
    Pt3(-1, 0, 0),
    Pt3(1, 0, 0),
    Pt3(0, -1, 0),
    Pt3(0, 1, 0),
    Pt3(0, 0, -1),
    Pt3(0, 0, 1),
  ];

  Pt3 min = Pt3(9999, 9999, 9999);
  Pt3 max = Pt3(-9999, -9999, -9999);

  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  for (String line in lines) {
    if (line.isEmpty) continue;

    List<int> coords = line.split(',').map(int.parse).toList();
    Pt3 cube = Pt3(coords[0], coords[1], coords[2]);
    cubes.add(cube);

    min = Pt3(
      min.x < cube.x ? min.x : cube.x,
      min.y < cube.y ? min.y : cube.y,
      min.z < cube.z ? min.z : cube.z,
    );
    max = Pt3(
      max.x > cube.x ? max.x : cube.x,
      max.y > cube.y ? max.y : cube.y,
      max.z > cube.z ? max.z : cube.z,
    );
  }

  min = min.add(Pt3(-1, -1, -1));
  max = max.add(Pt3(1, 1, 1));

  int faces = 0;
  Set<Pt3> seen = {min};
  List<Pt3> queue = [min];

  while (queue.isNotEmpty) {
    Pt3 curr = queue.removeAt(0);
    for (Pt3 delta in neighbors) {
      Pt3 next = curr.add(delta);

      if (next.x < min.x ||
          next.y < min.y ||
          next.z < min.z ||
          next.x > max.x ||
          next.y > max.y ||
          next.z > max.z) {
        continue;
      }

      if (cubes.contains(next)) {
        faces++;
      } else if (!seen.contains(next)) {
        seen.add(next);
        queue.add(next);
      }
    }
  }

  print(faces);
}
