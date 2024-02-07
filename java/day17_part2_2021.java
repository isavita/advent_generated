
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;

public class solution {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line = br.readLine();
            String[] parts = line.split(", ");
            String[] xRange = parts[0].substring(15).split("\\.\\.");
            String[] yRange = parts[1].substring(2).split("\\.\\.");
            int xMin = Integer.parseInt(xRange[0]);
            int xMax = Integer.parseInt(xRange[1]);
            int yMin = Integer.parseInt(yRange[0]);
            int yMax = Integer.parseInt(yRange[1]);

            HashSet<String> velocities = new HashSet<>();
            for (int xVel = -1000; xVel <= 1000; xVel++) {
                for (int yVel = -1000; yVel <= 1000; yVel++) {
                    int xPos = 0, yPos = 0;
                    int curXVel = xVel, curYVel = yVel;
                    boolean inTargetArea = false;
                    while (true) {
                        xPos += curXVel;
                        yPos += curYVel;

                        if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
                            inTargetArea = true;
                            break;
                        }

                        if (isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax)) {
                            break;
                        }

                        if (curXVel > 0) {
                            curXVel--;
                        } else if (curXVel < 0) {
                            curXVel++;
                        }

                        curYVel--;
                    }

                    if (inTargetArea) {
                        velocities.add(xVel + "," + yVel);
                    }
                }
            }

            System.out.println(velocities.size());
        } catch (IOException e) {
            System.out.println("Error opening file: " + e.getMessage());
            System.exit(1);
        }
    }

    private static boolean isMovingAway(int xPos, int yPos, int xVel, int yVel, int xMin, int xMax, int yMin, int yMax) {
        if (xPos < xMin && xVel < 0) {
            return true;
        }
        if (xPos > xMax && xVel > 0) {
            return true;
        }
        if (yPos < yMin && yVel < 0) {
            return true;
        }
        return false;
    }
}
