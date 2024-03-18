import 'dart:io';
import 'dart:math';

class Coord {
  int x, y, z;
  Coord(this.x, this.y, this.z);
}

class Brick {
  Coord mini, maxi;
  List<Brick> basedOn = [];
  List<Brick> support = [];
  Brick(this.mini, this.maxi);
}

List<Brick> parseInput(List<String> input) {
  List<Brick> bricks = [];
  for (var line in input) {
    List<int> coords = line.split('~').expand((s) => s.split(',').map(int.parse)).toList();
    bricks.add(Brick(Coord(coords[0], coords[1], coords[2]), Coord(coords[3], coords[4], coords[5])));
  }
  return bricks;
}

void settle(List<Brick> bricks) {
  bricks.sort((a, b) => a.maxi.z.compareTo(b.maxi.z));
  for (var brick in bricks) {
    int supportZ = 0;
    List<Brick> basedBricks = [];
    for (var i = bricks.indexOf(brick) - 1; i >= 0; i--) {
      bool isIntersectingX = max(brick.mini.x, bricks[i].mini.x) <= min(brick.maxi.x, bricks[i].maxi.x);
      bool isIntersectingY = max(brick.mini.y, bricks[i].mini.y) <= min(brick.maxi.y, bricks[i].maxi.y);
      if (isIntersectingX && isIntersectingY) {
        if (bricks[i].maxi.z == supportZ) {
          basedBricks.add(bricks[i]);
        } else if (bricks[i].maxi.z > supportZ) {
          supportZ = bricks[i].maxi.z;
          basedBricks = [bricks[i]];
        }
      }
    }
    brick.basedOn = basedBricks;
    for (var basedBrick in basedBricks) {
      basedBrick.support.add(brick);
    }
    int deltaZ = brick.maxi.z - brick.mini.z;
    brick.mini.z = supportZ + 1;
    brick.maxi.z = brick.mini.z + deltaZ;
  }
}

int solve(List<String> input) {
  List<Brick> bricks = parseInput(input);
  settle(bricks);
  int cnt = 0;
  for (var brick in bricks) {
    bool isDisintegratable = true;
    for (var supportedBrick in brick.support) {
      if (supportedBrick.basedOn.length < 2) {
        isDisintegratable = false;
        break;
      }
    }
    if (isDisintegratable) {
      cnt++;
    }
  }
  return cnt;
}

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  print(solve(input));
}