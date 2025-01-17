
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class ReindeerOlympics {

    static class Reindeer {
        String name;
        int speed;
        int flyTime;
        int restTime;
        int distance = 0;
        int points = 0;
        int cycleTime;
        int currentCycleTime = 0;
        boolean isFlying = true;

        public Reindeer(String name, int speed, int flyTime, int restTime) {
            this.name = name;
            this.speed = speed;
            this.flyTime = flyTime;
            this.restTime = restTime;
            this.cycleTime = flyTime + restTime;
        }

        public void move() {
            currentCycleTime++;
            if (isFlying) {
                distance += speed;
            }
            if (currentCycleTime == flyTime && isFlying) {
                isFlying = false;
                currentCycleTime = 0;
            } else if (currentCycleTime == restTime && !isFlying) {
                isFlying = true;
                currentCycleTime = 0;
            }
        }
    }

    public static void main(String[] args) {
        List<Reindeer> reindeers = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" ");
                reindeers.add(new Reindeer(parts[0], Integer.parseInt(parts[3]),
                        Integer.parseInt(parts[6]), Integer.parseInt(parts[13])));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        int raceTime = 2503;
        for (int i = 0; i < raceTime; i++) {
            int maxDistance = 0;
            for (Reindeer reindeer : reindeers) {
                reindeer.move();
                maxDistance = Math.max(maxDistance, reindeer.distance);
            }
            for (Reindeer reindeer : reindeers) {
                if (reindeer.distance == maxDistance) {
                    reindeer.points++;
                }
            }
        }

        int maxDistance = 0;
        int maxPoints = 0;
        for (Reindeer reindeer : reindeers) {
            maxDistance = Math.max(maxDistance, reindeer.distance);
            maxPoints = Math.max(maxPoints, reindeer.points);
        }

        System.out.println("Part 1: " + maxDistance);
        System.out.println("Part 2: " + maxPoints);
    }
}
