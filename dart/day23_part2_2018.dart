
import 'dart:io';
import 'dart:math';

class Nanobot {
  int x, y, z, r;

  Nanobot(this.x, this.y, this.z, this.r);

  int manhattanDistance(int x, int y, int z) {
    return (this.x - x).abs() + (this.y - y).abs() + (this.z - z).abs();
  }
}

void main() {
  List<Nanobot> nanobots = [];
  File('input.txt').readAsLinesSync().forEach((line) {
    RegExp exp = RegExp(r"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)");
    Match match = exp.firstMatch(line)!;
    nanobots.add(Nanobot(
      int.parse(match.group(1)!),
      int.parse(match.group(2)!),
      int.parse(match.group(3)!),
      int.parse(match.group(4)!),
    ));
  });

  // Part 1
  Nanobot strongest = nanobots.reduce((a, b) => a.r > b.r ? a : b);
  int inRangeCount = nanobots.where((bot) => bot.manhattanDistance(strongest.x, strongest.y, strongest.z) <= strongest.r).length;
  print(inRangeCount);

  // Part 2
  int minX = nanobots.map((bot) => bot.x).reduce(min);
  int maxX = nanobots.map((bot) => bot.x).reduce(max);
  int minY = nanobots.map((bot) => bot.y).reduce(min);
  int maxY = nanobots.map((bot) => bot.y).reduce(max);
  int minZ = nanobots.map((bot) => bot.z).reduce(min);
  int maxZ = nanobots.map((bot) => bot.z).reduce(max);

  int bestX = 0, bestY = 0, bestZ = 0, maxInRange = 0;
  int scale = 1;
  while (scale <= maxX - minX) {
    scale *= 2;
  }

  while (scale > 0) {
    maxInRange = 0;
    for (int x = minX; x <= maxX; x += scale) {
      for (int y = minY; y <= maxY; y += scale) {
        for (int z = minZ; z <= maxZ; z += scale) {
          int count = 0;
          for (Nanobot bot in nanobots) {
            if ((bot.manhattanDistance(x, y, z) - bot.r) ~/ scale <= 0) {
              count++;
            }
          }

          if (count > maxInRange) {
            maxInRange = count;
            bestX = x;
            bestY = y;
            bestZ = z;
          } else if (count == maxInRange) {
            if ((x.abs() + y.abs() + z.abs()) < (bestX.abs() + bestY.abs() + bestZ.abs())) {
              bestX = x;
              bestY = y;
              bestZ = z;
            }
          }
        }
      }
    }
    minX = bestX - scale;
    maxX = bestX + scale;
    minY = bestY - scale;
    maxY = bestY + scale;
    minZ = bestZ - scale;
    maxZ = bestZ + scale;
    
    scale ~/= 2;
  }

  print(bestX.abs() + bestY.abs() + bestZ.abs());
}
