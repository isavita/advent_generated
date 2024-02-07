
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class solution {
    public static void main(String[] args) {
        try {
            Map<String, Map<String, Integer>> distances = readAndParseInput("input.txt");
            List<String> locations = getUniqueLocations(distances);
            int maxDistance = findLongestRoute(locations, distances);
            System.out.println(maxDistance);
        } catch (FileNotFoundException e) {
            System.out.println("Error reading input: " + e.getMessage());
        }
    }

    public static Map<String, Map<String, Integer>> readAndParseInput(String filename) throws FileNotFoundException {
        File file = new File(filename);
        Scanner scanner = new Scanner(file);
        Map<String, Map<String, Integer>> distances = new HashMap<>();

        while (scanner.hasNextLine()) {
            String[] parts = scanner.nextLine().split(" ");
            if (parts.length != 5) {
                continue; // Invalid line format
            }

            String from = parts[0];
            String to = parts[2];
            int distance = Integer.parseInt(parts[4]);

            distances.computeIfAbsent(from, k -> new HashMap<>()).put(to, distance);
            distances.computeIfAbsent(to, k -> new HashMap<>()).put(from, distance); // Assuming distance is symmetric
        }

        return distances;
    }

    public static List<String> getUniqueLocations(Map<String, Map<String, Integer>> distances) {
        Set<String> locationSet = new HashSet<>();
        for (String from : distances.keySet()) {
            locationSet.add(from);
            for (String to : distances.get(from).keySet()) {
                locationSet.add(to);
            }
        }
        return new ArrayList<>(locationSet);
    }

    public static int findLongestRoute(List<String> locations, Map<String, Map<String, Integer>> distances) {
        int[] maxDistance = {0};
        permute(locations, 0, maxDistance, distances, false);
        return maxDistance[0];
    }

    public static void permute(List<String> arr, int i, int[] bestDistance, Map<String, Map<String, Integer>> distances, boolean findShortest) {
        if (i > arr.size()) {
            return;
        }
        if (i == arr.size()) {
            int dist = calculateRouteDistance(arr, distances);
            if (findShortest) {
                if (bestDistance[0] == 0 || dist < bestDistance[0]) {
                    bestDistance[0] = dist;
                }
            } else {
                if (dist > bestDistance[0]) {
                    bestDistance[0] = dist;
                }
            }
            return;
        }
        for (int j = i; j < arr.size(); j++) {
            Collections.swap(arr, i, j);
            permute(arr, i+1, bestDistance, distances, findShortest);
            Collections.swap(arr, i, j);
        }
    }

    public static int calculateRouteDistance(List<String> route, Map<String, Map<String, Integer>> distances) {
        int sum = 0;
        for (int i = 0; i < route.size()-1; i++) {
            sum += distances.get(route.get(i)).get(route.get(i+1));
        }
        return sum;
    }
}
