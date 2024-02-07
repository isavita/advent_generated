
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    static class Ship {
        int x, y;
        int waypointX;
        int waypointY;

        public Ship(int x, int y, int waypointX, int waypointY) {
            this.x = x;
            this.y = y;
            this.waypointX = waypointX;
            this.waypointY = waypointY;
        }

        public void processInstruction(char action, int value) {
            switch (action) {
                case 'N':
                    waypointY += value;
                    break;
                case 'S':
                    waypointY -= value;
                    break;
                case 'E':
                    waypointX += value;
                    break;
                case 'W':
                    waypointX -= value;
                    break;
                case 'L':
                    rotateWaypoint(-value);
                    break;
                case 'R':
                    rotateWaypoint(value);
                    break;
                case 'F':
                    x += waypointX * value;
                    y += waypointY * value;
                    break;
            }
        }

        public void rotateWaypoint(int degrees) {
            degrees = (degrees + 360) % 360;
            switch (degrees) {
                case 90:
                case -270:
                    int temp = waypointX;
                    waypointX = waypointY;
                    waypointY = -temp;
                    break;
                case 180:
                case -180:
                    waypointX = -waypointX;
                    waypointY = -waypointY;
                    break;
                case 270:
                case -90:
                    int temp2 = waypointX;
                    waypointX = -waypointY;
                    waypointY = temp2;
                    break;
            }
        }
    }

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            Ship ship = new Ship(0, 0, 10, 1);
            String line;
            while ((line = reader.readLine()) != null) {
                char action = line.charAt(0);
                int value = Integer.parseInt(line.substring(1));
                ship.processInstruction(action, value);
            }
            reader.close();
            int manhattanDistance = Math.abs(ship.x) + Math.abs(ship.y);
            System.out.println(manhattanDistance);
        } catch (IOException e) {
            System.out.println("Error opening file: " + e.getMessage());
        }
    }
}
