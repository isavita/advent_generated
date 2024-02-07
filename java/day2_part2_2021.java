
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int horizontalPosition = 0;
            int depth = 0;
            int aim = 0;

            String line;
            while ((line = reader.readLine()) != null) {
                String[] command = line.split(" ");
                String direction = command[0];
                int units = Integer.parseInt(command[1]);

                switch (direction) {
                    case "forward":
                        horizontalPosition += units;
                        depth += aim * units;
                        break;
                    case "down":
                        aim += units;
                        break;
                    case "up":
                        aim -= units;
                        break;
                }
            }

            int product = horizontalPosition * depth;
            System.out.println(product);

            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
