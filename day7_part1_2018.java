
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class solution {
    public static void main(String[] args) {
        Map<Character, List<Character>> deps = new HashMap<>();
        Map<Character, Boolean> allSteps = new HashMap<>();
        
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                char a = line.charAt(5);
                char b = line.charAt(36);
                if (!deps.containsKey(b)) {
                    deps.put(b, new ArrayList<>());
                }
                deps.get(b).add(a);
                allSteps.put(a, true);
                allSteps.put(b, true);
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        String order = topologicalSort(deps, allSteps);
        System.out.println(order);
    }

    public static String topologicalSort(Map<Character, List<Character>> deps, Map<Character, Boolean> allSteps) {
        List<Character> order = new ArrayList<>();
        List<Character> available = new ArrayList<>();

        for (char step : allSteps.keySet()) {
            if (deps.get(step) == null || deps.get(step).isEmpty()) {
                available.add(step);
            }
        }
        Collections.sort(available);

        while (!available.isEmpty()) {
            char next = available.get(0);
            available.remove(0);
            order.add(next);

            for (char step : allSteps.keySet()) {
                List<Character> dependencies = deps.get(step);
                if (dependencies != null && dependencies.contains(next)) {
                    dependencies.remove((Character) next);
                    if (dependencies.isEmpty()) {
                        available.add(step);
                    }
                }
            }
            Collections.sort(available);
        }

        StringBuilder result = new StringBuilder();
        for (char c : order) {
            result.append(c);
        }
        return result.toString();
    }
}
