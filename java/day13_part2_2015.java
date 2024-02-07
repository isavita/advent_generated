
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        Map<String, Map<String, Integer>> happinessMap = readHappinessValues("input.txt");
        addYourself(happinessMap);

        List<String> guests = getGuestList(happinessMap);
        int maxHappiness = calculateOptimalArrangement(guests, happinessMap);
        System.out.println(maxHappiness);
    }

    public static Map<String, Map<String, Integer>> readHappinessValues(String filename) {
        Map<String, Map<String, Integer>> happinessMap = new HashMap<>();
        try {
            File file = new File(filename);
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String[] parts = scanner.nextLine().split(" ");
                if (parts.length < 11) {
                    continue;
                }
                String from = parts[0];
                String to = parts[10].substring(0, parts[10].length() - 1);
                int change = Integer.parseInt(parts[3]);
                if (parts[2].equals("lose")) {
                    change = -change;
                }

                if (!happinessMap.containsKey(from)) {
                    happinessMap.put(from, new HashMap<>());
                }
                happinessMap.get(from).put(to, change);
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return happinessMap;
    }

    public static void addYourself(Map<String, Map<String, Integer>> happinessMap) {
        happinessMap.put("You", new HashMap<>());
        for (String guest : happinessMap.keySet()) {
            happinessMap.get(guest).put("You", 0);
            happinessMap.get("You").put(guest, 0);
        }
    }

    public static List<String> getGuestList(Map<String, Map<String, Integer>> happinessMap) {
        return new ArrayList<>(happinessMap.keySet());
    }

    public static int calculateOptimalArrangement(List<String> guests, Map<String, Map<String, Integer>> happinessMap) {
        int[] maxHappiness = {0};
        permute(guests, 0, maxHappiness, happinessMap);
        return maxHappiness[0];
    }

    public static void permute(List<String> arr, int i, int[] maxHappiness, Map<String, Map<String, Integer>> happinessMap) {
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

    public static int calculateHappiness(List<String> arrangement, Map<String, Map<String, Integer>> happinessMap) {
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
