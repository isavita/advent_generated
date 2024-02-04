
import 'dart:io';

void main() {
  List<String> grid = File('input.txt').readAsLinesSync();
  List<List<bool>> lights = List.generate(100, (_) => List.filled(100, false));

  for (int i = 0; i < grid.length; i++) {
    for (int j = 0; j < grid[i].length; j++) {
      lights[i][j] = grid[i][j] == '#';
    }
  }

  for (int step = 0; step < 100; step++) {
    List<List<bool>> newLights = List.generate(100, (_) => List.filled(100, false));

    for (int i = 0; i < 100; i++) {
      for (int j = 0; j < 100; j++) {
        int neighborsOn = 0;

        for (int ni = i - 1; ni <= i + 1; ni++) {
          for (int nj = j - 1; nj <= j + 1; nj++) {
            if (ni >= 0 && ni < 100 && nj >= 0 && nj < 100 && (ni != i || nj != j)) {
              if (lights[ni][nj]) {
                neighborsOn++;
              }
            }
          }
        }

        if (lights[i][j]) {
          newLights[i][j] = neighborsOn == 2 || neighborsOn == 3;
        } else {
          newLights[i][j] = neighborsOn == 3;
        }
      }
    }

    lights = newLights;
  }

  int totalLightsOn = 0;
  for (int i = 0; i < 100; i++) {
    for (int j = 0; j < 100; j++) {
      if (lights[i][j]) {
        totalLightsOn++;
      }
    }
  }

  print(totalLightsOn);
}
