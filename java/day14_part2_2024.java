
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RobotMovement {

  static class Robot {
    int x, y, vx, vy;

    Robot(int x, int y, int vx, int vy) {
      this.x = x;
      this.y = y;
      this.vx = vx;
      this.vy = vy;
    }
  }

  private static int mod(int a, int b) {
    return (a % b + b) % b;
  }

  private static Robot parseLine(String line) {
    Pattern pattern = Pattern.compile("p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)");
    Matcher matcher = pattern.matcher(line);
    if (!matcher.matches()) {
      throw new IllegalArgumentException("Invalid line format: " + line);
    }
    int x = Integer.parseInt(matcher.group(1));
    int y = Integer.parseInt(matcher.group(2));
    int vx = Integer.parseInt(matcher.group(3));
    int vy = Integer.parseInt(matcher.group(4));
    return new Robot(x, y, vx, vy);
  }

  private static List<Robot> moveRobots(List<Robot> robots, int sizeX, int sizeY) {
    List<Robot> nextRobots = new ArrayList<>();
    for (Robot robot : robots) {
      int nextX = mod(robot.x + robot.vx, sizeX);
      int nextY = mod(robot.y + robot.vy, sizeY);
      nextRobots.add(new Robot(nextX, nextY, robot.vx, robot.vy));
    }
    return nextRobots;
  }

  private static int[] countQuadrants(List<Robot> robots, int sizeX, int sizeY) {
    int[] counts = new int[4];
    int centerX = sizeX / 2;
    int centerY = sizeY / 2;
    for (Robot robot : robots) {
      if (robot.x < centerX) {
        if (robot.y < centerY) {
          counts[0]++;
        } else if (robot.y > centerY) {
          counts[1]++;
        }
      } else if (robot.x > centerX) {
        if (robot.y < centerY) {
          counts[2]++;
        } else if (robot.y > centerY) {
          counts[3]++;
        }
      }
    }
    return counts;
  }

  private static boolean hasNoOverlaps(List<Robot> robots) {
    Set<Long> positions = new HashSet<>();
    for (Robot robot : robots) {
      long key = ((long) robot.x << 32) | (robot.y & 0xFFFFFFFFL);
      if (!positions.add(key)) {
        return false;
      }
    }
    return true;
  }

  private static void drawGrid(List<Robot> robots, int sizeX, int sizeY) {
    char[][] grid = new char[sizeY][sizeX];
    for (int i = 0; i < sizeY; i++) {
      for (int j = 0; j < sizeX; j++) {
        grid[i][j] = '.';
      }
    }

    for (Robot robot : robots) {
      if (robot.y >= 0 && robot.y < sizeY && robot.x >= 0 && robot.x < sizeX) {
        grid[robot.y][robot.x] = '#';
      }
    }
    for (char[] row : grid) {
      System.out.println(new String(row));
    }
  }

  public static void main(String[] args) throws IOException {
    int sizeX = 101;
    int sizeY = 103;
    List<Robot> robots = new ArrayList<>();
    try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
      String line;
      while ((line = br.readLine()) != null) {
        line = line.trim();
        if (!line.isEmpty()) {
          robots.add(parseLine(line));
        }
      }
    }

    List<Robot> robotsPart1 = new ArrayList<>(robots);
    for (int i = 0; i < 100; i++) {
      robotsPart1 = moveRobots(robotsPart1, sizeX, sizeY);
    }
    int[] counts = countQuadrants(robotsPart1, sizeX, sizeY);
    long safetyFactor = 1;
    for (int c : counts) {
      safetyFactor *= c;
    }
    System.out.println("Part 1 - Safety Factor after 100 seconds: " + safetyFactor);

    List<Robot> robotsPart2 = new ArrayList<>(robots);
    int seconds = 0;
    while (true) {
      if (hasNoOverlaps(robotsPart2)) {
        break;
      }
      robotsPart2 = moveRobots(robotsPart2, sizeX, sizeY);
      seconds++;
      if (seconds > 1000000) {
        System.out.println("Exceeded maximum iterations without finding a unique position configuration.");
        return;
      }
    }
    System.out.println("Part 2 - Fewest seconds to display Easter egg: " + seconds);
    System.out.println("Final positions of robots:");
    drawGrid(robotsPart2, sizeX, sizeY);
  }
}
