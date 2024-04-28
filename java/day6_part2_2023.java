import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    public static long calculateWaysToWinLongRace(long time, long record) {
        long waysToWin = 0;
        for (long holdTime = 1; holdTime < time; holdTime++) {
            long travelTime = time - holdTime;
            long distance = holdTime * travelTime;
            if (distance > record) {
                waysToWin++;
            }
        }
        return waysToWin;
    }

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            long time = 0, distance = 0;
            int count = 0;
            while ((line = br.readLine()) != null) {
                if (!line.trim().isEmpty()) {
                    String[] parts = line.split(":");
                    String num = parts[1].replaceAll("\\s", "");
                    if (count == 0) {
                        time = Long.parseLong(num);
                    } else {
                        distance = Long.parseLong(num);
                    }
                    count++;
                }
            }
            long waysToWin = calculateWaysToWinLongRace(time, distance);
            System.out.println(waysToWin);
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }
}