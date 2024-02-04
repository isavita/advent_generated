
import 'dart:io';

class RebootStep {
  String action;
  int xStart;
  int xEnd;
  int yStart;
  int yEnd;
  int zStart;
  int zEnd;

  RebootStep(this.action, this.xStart, this.xEnd, this.yStart, this.yEnd, this.zStart, this.zEnd);
}

void main() {
  File file = File('input.txt');
  List<RebootStep> rebootSteps = [];

  file.readAsLinesSync().forEach((line) {
    if (line.isNotEmpty) {
      RebootStep step = parseRebootStep(line);
      rebootSteps.add(step);
    }
  });

  int minCoord = -50;
  int maxCoord = 50;
  List<List<List<bool>>> cubeGrid = createCubeGrid(minCoord, maxCoord);
  executeRebootSteps(cubeGrid, rebootSteps);
  int onCubes = countOnCubes(cubeGrid);

  print(onCubes);
}

RebootStep parseRebootStep(String line) {
  List<String> parts = line.split(' ');

  String action = parts[0];
  List<String> xRange = parts[1].split(',')[0].substring(2).split('..');
  List<String> yRange = parts[1].split(',')[1].substring(2).split('..');
  List<String> zRange = parts[1].split(',')[2].substring(2).split('..');

  int xStart = int.parse(xRange[0]);
  int xEnd = int.parse(xRange[1]);
  int yStart = int.parse(yRange[0]);
  int yEnd = int.parse(yRange[1]);
  int zStart = int.parse(zRange[0]);
  int zEnd = int.parse(zRange[1]);

  return RebootStep(action, xStart, xEnd, yStart, yEnd, zStart, zEnd);
}

List<List<List<bool>>> createCubeGrid(int minCoord, int maxCoord) {
  int gridSize = maxCoord - minCoord + 1;
  List<List<List<bool>>> grid = List.generate(gridSize, (i) => List.generate(gridSize, (j) => List.filled(gridSize, false)));

  return grid;
}

void executeRebootSteps(List<List<List<bool>>> cubeGrid, List<RebootStep> rebootSteps) {
  for (RebootStep step in rebootSteps) {
    if (!(step.xStart >= -50 && step.xEnd <= 50 && step.yStart >= -50 && step.yEnd <= 50 && step.zStart >= -50 && step.zEnd <= 50)) {
      continue;
    }
    for (int x = step.xStart; x <= step.xEnd; x++) {
      for (int y = step.yStart; y <= step.yEnd; y++) {
        for (int z = step.zStart; z <= step.zEnd; z++) {
          cubeGrid[x + 50][y + 50][z + 50] = step.action == 'on';
        }
      }
    }
  }
}

int countOnCubes(List<List<List<bool>>> cubeGrid) {
  int count = 0;

  for (int i = 0; i < cubeGrid.length; i++) {
    for (int j = 0; j < cubeGrid[i].length; j++) {
      for (int k = 0; k < cubeGrid[i][j].length; k++) {
        if (cubeGrid[i][j][k]) {
          count++;
        }
      }
    }
  }

  return count;
}
