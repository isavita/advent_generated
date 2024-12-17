
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Solution {

    static class Record {
        LocalDateTime time;
        String event;

        public Record(LocalDateTime time, String event) {
            this.time = time;
            this.event = event;
        }
    }

    static class Guard {
        int id;
        int[] minutes = new int[60];
        int totalMin = 0;

        public Guard(int id) {
            this.id = id;
        }
    }

    public static void main(String[] args) {
        List<Record> records = new ArrayList<>();
        Map<Integer, Guard> guards = new HashMap<>();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                LocalDateTime time = LocalDateTime.parse(line.substring(1, 17), formatter);
                String event = line.substring(19);
                records.add(new Record(time, event));
            }
        } catch (IOException e) {
            e.printStackTrace();
            return;
        }

        records.sort(Comparator.comparing(r -> r.time));

        Guard currentGuard = null;
        int sleepStart = 0;

        for (Record record : records) {
            String event = record.event;
            if (event.contains("begins shift")) {
                int id = Integer.parseInt(event.split(" ")[1].substring(1));
                currentGuard = guards.computeIfAbsent(id, Guard::new);
            } else if (event.contains("falls asleep")) {
                sleepStart = record.time.getMinute();
            } else if (event.contains("wakes up")) {
                int wakeUp = record.time.getMinute();
                for (int i = sleepStart; i < wakeUp; i++) {
                    currentGuard.minutes[i]++;
                    currentGuard.totalMin++;
                }
            }
        }

        Guard mostFreqGuard = null;
        int mostFreqMin = 0;
        int maxMinutes = 0;

        for (Guard g : guards.values()) {
            for (int i = 0; i < 60; i++) {
                if (g.minutes[i] > maxMinutes) {
                    maxMinutes = g.minutes[i];
                    mostFreqGuard = g;
                    mostFreqMin = i;
                }
            }
        }

        System.out.println(mostFreqGuard.id * mostFreqMin);
    }
}
