
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Scanner;
import java.util.Set;

public class OrbitMap {

    public static void main(String[] args) {
        try {
            File inputFile = new File("input.txt");
            Scanner scanner = new Scanner(inputFile);
            Map<String, String> orbits = new HashMap<>();

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] parts = line.split("\\)");
                orbits.put(parts[1], parts[0]);
            }
            scanner.close();

            int totalOrbits = calculateTotalOrbits(orbits);
            System.out.println(totalOrbits);

            int transfers = calculateOrbitalTransfers(orbits, "YOU", "SAN");
            System.out.println(transfers);

        } catch (FileNotFoundException e) {
            System.out.println("File not found: " + e.getMessage());
        }
    }

    private static int calculateTotalOrbits(Map<String, String> orbits) {
        int totalOrbits = 0;
        for (String object : orbits.keySet()) {
            String current = object;
            while (orbits.containsKey(current)) {
                totalOrbits++;
                current = orbits.get(current);
            }
        }
        return totalOrbits;
    }

    private static int calculateOrbitalTransfers(Map<String, String> orbits, String start, String end) {
        String startOrbiting = orbits.get(start);
        String endOrbiting = orbits.get(end);

        if (startOrbiting == null || endOrbiting == null) {
            return -1;
        }

        Map<String, Integer> distances = new HashMap<>();
        Queue<String> queue = new LinkedList<>();
        Set<String> visited = new HashSet<>();

        queue.add(startOrbiting);
        distances.put(startOrbiting, 0);
        visited.add(startOrbiting);

        while (!queue.isEmpty()) {
            String current = queue.poll();

            if (current.equals(endOrbiting)) {
                return distances.get(current);
            }

            // Check orbiting object
            if (orbits.containsKey(current)) {
                String parent = orbits.get(current);
                if (!visited.contains(parent)) {
                    queue.add(parent);
                    distances.put(parent, distances.get(current) + 1);
                    visited.add(parent);
                }
            }

            // Check objects that orbit current
            for (Map.Entry<String, String> entry : orbits.entrySet()) {
                if (entry.getValue().equals(current)) {
                    String child = entry.getKey();
                    if (!visited.contains(child)) {
                        queue.add(child);
                        distances.put(child, distances.get(current) + 1);
                        visited.add(child);
                    }
                }
            }
        }
        return -1;
    }
}
