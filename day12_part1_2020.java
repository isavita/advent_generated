
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            Ship ship = new Ship(0, 0, 0);
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                char action = line.charAt(0);
                int value = Integer.parseInt(line.substring(1));
                ship.processInstruction(action, value);
            }

            int manhattanDistance = Math.abs(ship.x) + Math.abs(ship.y);
            System.out.println(manhattanDistance);

        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e.getMessage());
        }
    }

    static class Ship {
        int x, y, facing;

        Ship(int x, int y, int facing) {
            this.x = x;
            this.y = y;
            this.facing = facing;
        }

        void processInstruction(char action, int value) {
            switch (action) {
                case 'N':
                    y += value;
                    break;
                case 'S':
                    y -= value;
                    break;
                case 'E':
                    x += value;
                    break;
                case 'W':
                    x -= value;
                    break;
                case 'L':
                    facing = (facing - value + 360) % 360;
                    break;
                case 'R':
                    facing = (facing + value) % 360;
                    break;
                case 'F':
                    switch (facing) {
                        case 0:
                            x += value;
                            break;
                        case 90:
                            y -= value;
                            break;
                        case 180:
                            x -= value;
                            break;
                        case 270:
                            y += value;
                            break;
                    }
                    break;
            }
        }
    }
}
