
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Lanternfish {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
        String line = reader.readLine();
        reader.close();

        long[] lanternFishCounts = new long[9];
        for (String age : line.split(",")) {
            lanternFishCounts[Integer.parseInt(age)]++;
        }

        for (int i = 0; i < 256; i++) {
            long newLanternFish = lanternFishCounts[0];
            System.arraycopy(lanternFishCounts, 1, lanternFishCounts, 0, 8);
            lanternFishCounts[6] += newLanternFish;
            lanternFishCounts[8] = newLanternFish;
        }

        long total = 0;
        for (long count : lanternFishCounts) {
            total += count;
        }
        System.out.println(total);
    }
}
