import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

public class Main {
    static class Record implements Comparable<Record> {
        LocalDateTime timestamp;
        String action;
        int guardID;

        public Record(LocalDateTime timestamp, String action, int guardID) {
            this.timestamp = timestamp;
            this.action = action;
            this.guardID = guardID;
        }

        @Override
        public int compareTo(Record o) {
            return this.timestamp.compareTo(o.timestamp);
        }
    }

    public static void main(String[] args) throws IOException {
        List<Record> records = readAndParseInput("input.txt");
        Collections.sort(records);

        Map<Integer, int[]> guardSleepMinutes = new HashMap<>();
        int currentGuardID = 0;
        LocalDateTime sleepStart = null;

        for (Record record : records) {
            switch (record.action) {
                case "begins shift":
                    currentGuardID = record.guardID;
                    break;
                case "falls asleep":
                    sleepStart = record.timestamp;
                    break;
                case "wakes up":
                    if (!guardSleepMinutes.containsKey(currentGuardID)) {
                        guardSleepMinutes.put(currentGuardID, new int[60]);
                    }
                    for (int i = sleepStart.getMinute(); i < record.timestamp.getMinute(); i++) {
                        guardSleepMinutes.get(currentGuardID)[i]++;
                    }
                    break;
            }
        }

        int maxSleep = 0;
        int sleepiestGuard = 0;
        for (Map.Entry<Integer, int[]> entry : guardSleepMinutes.entrySet()) {
            int totalSleep = 0;
            for (int count : entry.getValue()) {
                totalSleep += count;
            }
            if (totalSleep > maxSleep) {
                maxSleep = totalSleep;
                sleepiestGuard = entry.getKey();
            }
        }

        int maxMinute = 0;
        int maxMinuteCount = 0;
        for (int i = 0; i < 60; i++) {
            if (guardSleepMinutes.get(sleepiestGuard)[i] > maxMinuteCount) {
                maxMinuteCount = guardSleepMinutes.get(sleepiestGuard)[i];
                maxMinute = i;
            }
        }

        System.out.println(sleepiestGuard * maxMinute);
    }

    static List<Record> readAndParseInput(String filename) throws IOException {
        List<Record> records = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
            while ((line = br.readLine()) != null) {
                String[] parts = line.split("\\] ");
                LocalDateTime ts = LocalDateTime.parse(parts[0].substring(1), formatter);
                String actionPart = parts[1];

                int guardID = -1;
                if (actionPart.contains("Guard")) {
                    String[] actionParts = actionPart.split(" ");
                    guardID = Integer.parseInt(actionParts[1].substring(1));
                    actionPart = "begins shift";
                } else if (actionPart.contains("falls asleep")) {
                    actionPart = "falls asleep";
                } else if (actionPart.contains("wakes up")) {
                    actionPart = "wakes up";
                }

                records.add(new Record(ts, actionPart, guardID));
            }
        }
        return records;
    }
}