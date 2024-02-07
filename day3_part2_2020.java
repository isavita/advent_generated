
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            StringBuilder sb = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                sb.append(line).append("\n");
            }
            String[] lines = sb.toString().split("\n");

            int[][] slopes = {
                    {1, 1},
                    {3, 1},
                    {5, 1},
                    {7, 1},
                    {1, 2}
            };

            int product = 1;
            for (int[] slope : slopes) {
                int treeCount = 0;
                int pos = 0;
                for (int i = 0; i < lines.length; i += slope[1]) {
                    if (lines[i].charAt(pos) == '#') {
                        treeCount++;
                    }
                    pos = (pos + slope[0]) % lines[i].length();
                }
                product *= treeCount;
            }

            System.out.println(product);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
