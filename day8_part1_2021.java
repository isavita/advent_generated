
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            int count = 0;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" \\| ");
                String output = parts[1];
                for (String digit : output.split(" ")) {
                    switch (digit.length()) {
                        case 2:
                        case 4:
                        case 3:
                        case 7:
                            count++;
                    }
                }
            }
            System.out.println(count);
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
