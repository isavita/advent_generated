
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int prev = 0;
            int current;
            int count = 0;

            String line;
            while ((line = reader.readLine()) != null) {
                current = Integer.parseInt(line);
                if (prev != 0 && current > prev) {
                    count++;
                }
                prev = current;
            }

            System.out.println(count);

            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }
}
