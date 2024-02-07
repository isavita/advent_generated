import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int count = 0;
            String line;
            while ((line = reader.readLine()) != null) {
                String[] pair = line.split(",");

                // Extract ranges
                int[] left = parseRange(pair[0]);
                int[] right = parseRange(pair[1]);

                // Check if ranges overlap
                if (left[0] <= right[1] && left[1] >= right[0]) {
                    count++;
                }
            }

            System.out.println(count);
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static int[] parseRange(String s) {
        String[] split = s.split("-");
        int start = Integer.parseInt(split[0]);
        int end = Integer.parseInt(split[1]);
        return new int[]{start, end};
    }
}