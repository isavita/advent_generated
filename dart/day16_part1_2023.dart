
import 'dart:io';
import 'dart:collection';

class Coord {
  final int x;
  final int y;
  
  const Coord(this.x, this.y);
  
  Coord operator +(Coord other) => Coord(x + other.x, y + other.y);
  
  Coord rotate90() => Coord(y, -x);
  Coord rotateNeg90() => Coord(-y, x);
  
  bool isInBounds(Grid grid) => 
    x >= 0 && x < grid.width && y >= 0 && y < grid.height;
  
  @override
  bool operator ==(Object other) => 
    other is Coord && x == other.x && y == other.y;
  
  @override
  int get hashCode => Object.hash(x, y);
}

class Beam {
  final Coord origin;
  final Coord dir;
  
  const Beam(this.origin, this.dir);
  
  @override
  bool operator ==(Object other) => 
    other is Beam && origin == other.origin && dir == other.dir;
  
  @override
  int get hashCode => Object.hash(origin, dir);
}

class Grid {
  final int width;
  final int height;
  final Map<Coord, String> data;
  
  const Grid(this.width, this.height, this.data);
}

class Solution {
  static final north = const Coord(0, -1);
  static final west = const Coord(-1, 0);
  static final south = const Coord(0, 1);
  static final east = const Coord(1, 0);

  static Grid buildGrid(List<String> input) {
    Map<Coord, String> data = {};
    for (int y = 0; y < input.length; y++) {
      for (int x = 0; x < input[y].length; x++) {
        if (input[y][x] != '.') {
          data[Coord(x, y)] = input[y][x];
        }
      }
    }
    return Grid(input[0].length, input.length, data);
  }

  static List<Beam> nextBeam(Grid grid, Beam beam) {
    List<Beam> beams = [];
    String? char = grid.data[beam.origin];

    if (char == null) {
      return [Beam(beam.origin + beam.dir, beam.dir)];
    }

    switch (char) {
      case '/':
        Coord newDir = (beam.dir == north || beam.dir == south) 
          ? beam.dir.rotateNeg90() 
          : beam.dir.rotate90();
        beams.add(Beam(beam.origin + newDir, newDir));
        break;
      
      case '\\':
        Coord newDir = (beam.dir == north || beam.dir == south) 
          ? beam.dir.rotate90() 
          : beam.dir.rotateNeg90();
        beams.add(Beam(beam.origin + newDir, newDir));
        break;
      
      case '|':
        if (beam.dir == east || beam.dir == west) {
          Coord newDir1 = beam.dir.rotate90();
          Coord newDir2 = beam.dir.rotateNeg90();
          beams.add(Beam(beam.origin + newDir1, newDir1));
          beams.add(Beam(beam.origin + newDir2, newDir2));
        } else {
          beams.add(Beam(beam.origin + beam.dir, beam.dir));
        }
        break;
      
      case '-':
        if (beam.dir == north || beam.dir == south) {
          Coord newDir1 = beam.dir.rotate90();
          Coord newDir2 = beam.dir.rotateNeg90();
          beams.add(Beam(beam.origin + newDir1, newDir1));
          beams.add(Beam(beam.origin + newDir2, newDir2));
        } else {
          beams.add(Beam(beam.origin + beam.dir, beam.dir));
        }
        break;
      
      default:
        beams.add(Beam(beam.origin + beam.dir, beam.dir));
    }

    return beams;
  }

  static int solve(List<String> input) {
    Grid grid = buildGrid(input);
    Beam start = Beam(Coord(0, 0), east);

    Set<Beam> alreadySeen = {};
    Queue<Beam> toExplore = Queue()..add(start);

    while (toExplore.isNotEmpty) {
      Beam beam = toExplore.removeFirst();

      if (beam.origin.isInBounds(grid) && !alreadySeen.contains(beam)) {
        alreadySeen.add(beam);
        toExplore.addAll(nextBeam(grid, beam));
      }
    }

    return alreadySeen.map((beam) => beam.origin).toSet().length;
  }
}

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  print(Solution.solve(input));
}
