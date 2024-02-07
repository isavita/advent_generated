
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader br = new BufferedReader(new FileReader("input.txt"));
            String line;
            int horizontalPosition = 0;
            int depth = 0;

            while ((line = br.readLine()) != null) {
                String[] command = line.split(" ");
                String direction = command[0];
                int units = Integer.parseInt(command[1]);

                switch (direction) {
                    case "forward":
                        horizontalPosition += units;
                        break;
                    case "down":
                        depth += units;
                        break;
                    case "up":
                        depth -= units;
                        break;
                }
            }

            int product = horizontalPosition * depth;
            System.out.println(product);

            br.close();
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}
