import java.io.*;
import java.util.*;

public class ShuttleSearch {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            int timestamp = scanner.nextInt();
            String[] buses = scanner.next().split(",");
            List<Integer> busIds = new ArrayList<>();
            for (String bus : buses) {
                if (!bus.equals("x")) {
                    busIds.add(Integer.parseInt(bus));
                }
            }

            // Part 1
            int minWait = Integer.MAX_VALUE;
            int busId = 0;
            for (int id : busIds) {
                int wait = id - (timestamp % id);
                if (wait < minWait) {
                    minWait = wait;
                    busId = id;
                }
            }
            System.out.println("Part 1: " + busId * minWait);

            // Part 2
            long timestamp2 = 0;
            long step = 1;
            for (int i = 0; i < buses.length; i++) {
                if (!buses[i].equals("x")) {
                    int id = Integer.parseInt(buses[i]);
                    while ((timestamp2 + i) % id != 0) {
                        timestamp2 += step;
                    }
                    step *= id;
                }
            }
            System.out.println("Part 2: " + timestamp2);
        } catch (FileNotFoundException e) {
            System.out.println("File not found");
        }
    }
}