
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Main {

    static int calculateWaysToWin(long time, long record) {
        long low = 0, high = time;
        while (low <= high) {
            long mid = low + (high - low) / 2;
            long distance = mid * (time - mid);
            if (distance > record) {
                high = mid - 1;
            } else {
                low = mid + 1;
            }
        }
        long firstWin = low;
        low = 0;
        high = time;
        while (low <= high) {
            long mid = low + (high - low) / 2;
            long distance = mid * (time - mid);
            if (distance > record) {
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
        long lastWin = high;
        return (int)(lastWin - firstWin + 1);
    }

    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            List<Long> times = new ArrayList<>();
            List<Long> distances = new ArrayList<>();
            String[] parts;
            if (scanner.hasNextLine()) {
                parts = scanner.nextLine().split("\\s+");
                for (int i = 1; i < parts.length; i++) {
                    times.add(Long.parseLong(parts[i]));
                }
            }
            if (scanner.hasNextLine()) {
                parts = scanner.nextLine().split("\\s+");
                for (int i = 1; i < parts.length; i++) {
                    distances.add(Long.parseLong(parts[i]));
                }
            }
            scanner.close();
            long totalWays = 1;
            for (int i = 0; i < times.size(); i++) {
                totalWays *= calculateWaysToWin(times.get(i), distances.get(i));
            }
            System.out.println(totalWays);
        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e.getMessage());
        }
    }
}
