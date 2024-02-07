
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    static class Position {
        int x, y, dirIndex;

        public Position(int x, int y, int dirIndex) {
            this.x = x;
            this.y = y;
            this.dirIndex = dirIndex;
        }
    }

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            String[] instructions = null;
            while ((line = reader.readLine()) != null) {
                instructions = line.split(", ");
            }
            reader.close();

            Position pos = new Position(0, 0, 0);
            int[][] directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};

            for (String instruction : instructions) {
                String turn = instruction.substring(0, 1);
                int blocks = Integer.parseInt(instruction.substring(1));

                if (turn.equals("R")) {
                    pos.dirIndex = (pos.dirIndex + 1) % 4;
                } else {
                    pos.dirIndex = (pos.dirIndex - 1 + 4) % 4;
                }

                pos.x += directions[pos.dirIndex][0] * blocks;
                pos.y += directions[pos.dirIndex][1] * blocks;
            }

            System.out.println(abs(pos.x) + abs(pos.y));

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static int abs(int x) {
        return (x < 0) ? -x : x;
    }
}
