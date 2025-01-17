
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class ReindeerOlympics {

    public static void main(String[] args) {
        try {
            List<Reindeer> reindeerList = readReindeerData("input.txt");
            int winningDistance = calculateWinningDistance(reindeerList, 2503);
            System.out.println(winningDistance);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    static class Reindeer {
        int speed;
        int flyTime;
        int restTime;

        public Reindeer(int speed, int flyTime, int restTime) {
            this.speed = speed;
            this.flyTime = flyTime;
            this.restTime = restTime;
        }
    }

    static List<Reindeer> readReindeerData(String filename) throws IOException {
        List<Reindeer> reindeerList = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" ");
                int speed = Integer.parseInt(parts[3]);
                int flyTime = Integer.parseInt(parts[6]);
                int restTime = Integer.parseInt(parts[13]);
                reindeerList.add(new Reindeer(speed, flyTime, restTime));
            }
        }
        return reindeerList;
    }

    static int calculateWinningDistance(List<Reindeer> reindeerList, int totalTime) {
        int maxDistance = 0;
        for (Reindeer reindeer : reindeerList) {
            int cycleTime = reindeer.flyTime + reindeer.restTime;
            int numFullCycles = totalTime / cycleTime;
            int remainingTime = totalTime % cycleTime;
            int distance = numFullCycles * reindeer.speed * reindeer.flyTime;
            distance += Math.min(remainingTime, reindeer.flyTime) * reindeer.speed;
            maxDistance = Math.max(maxDistance, distance);
        }
        return maxDistance;
    }
}
