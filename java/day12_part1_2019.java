
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class MoonSimulation {

  static void updateVelocity(int[][] moons, int[][] velocities) {
    for (int i = 0; i < moons.length; i++) {
      for (int j = i + 1; j < moons.length; j++) {
        for (int axis = 0; axis < 3; axis++) {
          if (moons[i][axis] < moons[j][axis]) {
            velocities[i][axis]++;
            velocities[j][axis]--;
          } else if (moons[i][axis] > moons[j][axis]) {
            velocities[i][axis]--;
            velocities[j][axis]++;
          }
        }
      }
    }
  }

  static void updatePositions(int[][] moons, int[][] velocities) {
    for (int i = 0; i < moons.length; i++) {
      for (int axis = 0; axis < 3; axis++) {
        moons[i][axis] += velocities[i][axis];
      }
    }
  }

  static int calculateEnergy(int[][] moons, int[][] velocities) {
    int totalEnergy = 0;
    for (int i = 0; i < moons.length; i++) {
      int potential = 0;
      int kinetic = 0;
      for (int axis = 0; axis < 3; axis++) {
        potential += Math.abs(moons[i][axis]);
        kinetic += Math.abs(velocities[i][axis]);
      }
      totalEnergy += potential * kinetic;
    }
    return totalEnergy;
  }

  public static void main(String[] args) throws IOException {
    Scanner scanner = new Scanner(new File("input.txt"));
    List<int[]> moonList = new ArrayList<>();
    while (scanner.hasNextLine()) {
      String line = scanner.nextLine().trim();
      line = line.substring(1, line.length() - 1);
      String[] coords = line.split(", ");
      int[] moon = new int[3];
      for (int i = 0; i < 3; i++) {
        moon[i] = Integer.parseInt(coords[i].split("=")[1]);
      }
      moonList.add(moon);
    }
    scanner.close();

    int[][] moons = moonList.toArray(new int[0][]);
    int[][] velocities = new int[moons.length][3];

    for (int step = 0; step < 1000; step++) {
      updateVelocity(moons, velocities);
      updatePositions(moons, velocities);
    }

    System.out.println(calculateEnergy(moons, velocities));
  }
}
