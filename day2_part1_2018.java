
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int twoCount = 0, threeCount = 0;
            String line;
            while ((line = reader.readLine()) != null) {
                boolean[] counts = countTwosAndThrees(line);
                if (counts[0]) {
                    twoCount++;
                }
                if (counts[1]) {
                    threeCount++;
                }
            }
            reader.close();

            int checksum = twoCount * threeCount;
            System.out.println(checksum);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static boolean[] countTwosAndThrees(String id) {
        int[] charCount = new int[256]; // assuming ASCII characters
        for (int i = 0; i < id.length(); i++) {
            charCount[id.charAt(i)]++;
        }

        boolean hasTwos = false, hasThrees = false;
        for (int count : charCount) {
            if (count == 2) {
                hasTwos = true;
            } else if (count == 3) {
                hasThrees = true;
            }
        }
        return new boolean[]{hasTwos, hasThrees};
    }
}
