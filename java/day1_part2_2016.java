
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashSet;
import java.util.Scanner;
import java.util.Set;

public class script {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            scanner.useDelimiter(", ");
            int x = 0;
            int y = 0;
            int dir = 0;
            Set<String> visited = new HashSet<>();
            visited.add("0,0");

            while (scanner.hasNext()) {
                String instruction = scanner.next();
                char turn = instruction.charAt(0);
                int steps = Integer.parseInt(instruction.substring(1));
                if (turn == 'R') {
                    dir = (dir + 1) % 4;
                } else {
                    dir = (dir + 3) % 4;
                }
                for (int i = 0; i < steps; i++) {
                    if (dir == 0) {
                        y++;
                    } else if (dir == 1) {
                        x++;
                    } else if (dir == 2) {
                        y--;
                    } else {
                        x--;
                    }
                    String position = x + "," + y;
                    if (visited.contains(position)) {
                        System.out.println(Math.abs(x) + Math.abs(y));
                        return;
                    }
                    visited.add(position);
                }
            }

            System.out.println(Math.abs(x) + Math.abs(y));
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }
}
