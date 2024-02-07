
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int[][] counts = new int[12][2];

            String line;
            while ((line = reader.readLine()) != null) {
                for (int i = 0; i < line.length(); i++) {
                    counts[i][line.charAt(i) - '0']++;
                }
            }

            int gammaRate = 0;
            int epsilonRate = 0;
            for (int i = 0; i < counts.length; i++) {
                if (counts[i][0] > counts[i][1]) {
                    gammaRate |= 1 << (counts.length - i - 1);
                } else {
                    epsilonRate |= 1 << (counts.length - i - 1);
                }
            }

            System.out.println(gammaRate * epsilonRate);
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
