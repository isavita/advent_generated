
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        HashMap<String, HashMap<String, Integer>> happinessMap = readHappinessValues("input.txt");
        List<String> guests = getGuestList(happinessMap);
        int maxHappiness = calculateOptimalArrangement(guests, happinessMap);
        System.out.println(maxHappiness);
    }

    private static HashMap<String, HashMap<String, Integer>> readHappinessValues(String filename) {
        HashMap<String, HashMap<String, Integer>> happinessMap = new HashMap<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" ");
                if (parts.length < 11) {
                    continue; // Skip invalid lines
                }
                String from = parts[0];
                String to = parts[10].substring(0, parts[10].length() - 1); // Trim period
                int change = Integer.parseInt(parts[3]);
                if (parts[2].equals("lose")) {
                    change = -change;
                }

                if (!happinessMap.containsKey(from)) {
                    happinessMap.put(from, new HashMap<>());
                }
                happinessMap.get(from).put(to, change);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return happinessMap;
    }

    private static List<String> getGuestList(HashMap<String, HashMap<String, Integer>> happinessMap) {
        return new ArrayList<>(happinessMap.keySet());
    }

    private static int calculateOptimalArrangement(List<String> guests, HashMap<String, HashMap<String, Integer>> happinessMap) {
        int[] maxHappiness = {0};
        permute(guests, 0, maxHappiness, happinessMap);
        return maxHappiness[0];
    }

    private static void permute(List<String> arr, int i, int[] maxHappiness, HashMap<String, HashMap<String, Integer>> happinessMap) {
        if (i > arr.size()) {
            return;
        }
        if (i == arr.size()) {
            int happiness = calculateHappiness(arr, happinessMap);
            if (happiness > maxHappiness[0]) {
                maxHappiness[0] = happiness;
            }
            return;
        }
        for (int j = i; j < arr.size(); j++) {
            String temp = arr.get(i);
            arr.set(i, arr.get(j));
            arr.set(j, temp);
            permute(arr, i + 1, maxHappiness, happinessMap);
            temp = arr.get(i);
            arr.set(i, arr.get(j));
            arr.set(j, temp);
        }
    }

    private static int calculateHappiness(List<String> arrangement, HashMap<String, HashMap<String, Integer>> happinessMap) {
        int happiness = 0;
        int n = arrangement.size();
        for (int i = 0; i < n; i++) {
            int left = (i + n - 1) % n;
            int right = (i + 1) % n;
            happiness += happinessMap.get(arrangement.get(i)).get(arrangement.get(left));
            happiness += happinessMap.get(arrangement.get(i)).get(arrangement.get(right));
        }
        return happiness;
    }
}
