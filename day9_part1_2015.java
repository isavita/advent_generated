
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        HashMap<String, HashMap<String, Integer>> distances = readAndParseInput("input.txt");
        ArrayList<String> locations = getUniqueLocations(distances);
        int minDistance = findShortestRoute(locations, distances);
        System.out.println(minDistance);
    }

    public static HashMap<String, HashMap<String, Integer>> readAndParseInput(String filename) {
        HashMap<String, HashMap<String, Integer>> distances = new HashMap<>();
        try {
            File file = new File(filename);
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String[] parts = scanner.nextLine().split(" ");
                if (parts.length != 5) {
                    continue;
                }

                String from = parts[0];
                String to = parts[2];
                int distance = Integer.parseInt(parts[4]);

                distances.putIfAbsent(from, new HashMap<>());
                distances.get(from).put(to, distance);

                distances.putIfAbsent(to, new HashMap<>());
                distances.get(to).put(from, distance);
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return distances;
    }

    public static ArrayList<String> getUniqueLocations(HashMap<String, HashMap<String, Integer>> distances) {
        HashSet<String> locationSet = new HashSet<>();
        for (String from : distances.keySet()) {
            locationSet.add(from);
            for (String to : distances.get(from).keySet()) {
                locationSet.add(to);
            }
        }
        return new ArrayList<>(locationSet);
    }

    public static int findShortestRoute(ArrayList<String> locations, HashMap<String, HashMap<String, Integer>> distances) {
        int[] minDistance = {-1};
        permute(locations, 0, minDistance, distances);
        return minDistance[0];
    }

    public static void permute(ArrayList<String> arr, int i, int[] minDistance, HashMap<String, HashMap<String, Integer>> distances) {
        if (i > arr.size()) {
            return;
        }
        if (i == arr.size()) {
            int dist = calculateRouteDistance(arr, distances);
            if (minDistance[0] == -1 || dist < minDistance[0]) {
                minDistance[0] = dist;
            }
            return;
        }
        for (int j = i; j < arr.size(); j++) {
            String temp = arr.get(i);
            arr.set(i, arr.get(j));
            arr.set(j, temp);
            permute(arr, i + 1, minDistance, distances);
            arr.set(j, arr.get(i));
            arr.set(i, temp);
        }
    }

    public static int calculateRouteDistance(ArrayList<String> route, HashMap<String, HashMap<String, Integer>> distances) {
        int sum = 0;
        for (int i = 0; i < route.size() - 1; i++) {
            sum += distances.get(route.get(i)).get(route.get(i + 1));
        }
        return sum;
    }
}
