
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class PageSorter {

    public static void main(String[] args) {
        List<int[]> orderingRules = new ArrayList<>();
        List<int[]> updates = new ArrayList<>();

        try (Scanner scanner = new Scanner(new File("input.txt"))) {
            boolean isUpdateSection = false;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine().trim();
                if (line.isEmpty()) {
                    isUpdateSection = true;
                    continue;
                }
                if (!isUpdateSection) {
                    String[] parts = line.split("\\|");
                    if (parts.length == 2) {
                        try {
                            int x = Integer.parseInt(parts[0].trim());
                            int y = Integer.parseInt(parts[1].trim());
                            orderingRules.add(new int[]{x, y});
                        } catch (NumberFormatException ignored) {}
                    }
                } else {
                    String[] nums = line.split(",");
                    int[] update = new int[nums.length];
                    int i = 0;
                    for (String numStr : nums) {
                        try {
                            update[i++] = Integer.parseInt(numStr.trim());
                        } catch (NumberFormatException ignored) {}
                    }
                    if (i > 0) {
                        updates.add(Arrays.copyOf(update, i));
                    }
                }
            }
        } catch (FileNotFoundException e) {
            System.err.println("Error reading input: " + e.getMessage());
            return;
        }


        long sum = 0;
        for (int[] update : updates) {
            if (!isCorrectlyOrdered(update, orderingRules)) {
                try {
                    int[] sortedUpdate = sortUpdate(update, orderingRules);
                    sum += sortedUpdate[sortedUpdate.length / 2];
                } catch (IllegalArgumentException e) {
                    System.err.println("Error sorting update: " + e.getMessage());
                }
            }
        }

        System.out.println(sum);
    }

    static boolean isCorrectlyOrdered(int[] update, List<int[]> rules) {
        Map<Integer, Integer> position = new HashMap<>();
        for (int i = 0; i < update.length; i++) {
            position.put(update[i], i);
        }
        for (int[] rule : rules) {
            int x = rule[0];
            int y = rule[1];
            if (position.containsKey(x) && position.containsKey(y)) {
                if (position.get(x) >= position.get(y)) {
                    return false;
                }
            }
        }
        return true;
    }

    static int[] sortUpdate(int[] update, List<int[]> rules) {
        Map<Integer, List<Integer>> adjacency = new HashMap<>();
        Set<Integer> pagesInUpdate = new HashSet<>();
        for (int page : update) {
            pagesInUpdate.add(page);
            adjacency.put(page, new ArrayList<>());
        }
        for (int[] rule : rules) {
            int x = rule[0];
            int y = rule[1];
            if (pagesInUpdate.contains(x) && pagesInUpdate.contains(y)) {
                adjacency.get(x).add(y);
            }
        }

        Set<Integer> visited = new HashSet<>();
        Set<Integer> tempMarked = new HashSet<>();
        List<Integer> result = new ArrayList<>();
        for (int page : pagesInUpdate) {
            if (!visited.contains(page)) {
                topologicalSort(adjacency, page, visited, tempMarked, result);
            }
        }
        Collections.reverse(result);
        return result.stream().mapToInt(Integer::intValue).toArray();
    }

    static void topologicalSort(Map<Integer, List<Integer>> adjacency, int node, Set<Integer> visited, Set<Integer> tempMarked, List<Integer> result) {
        if (tempMarked.contains(node)) {
            throw new IllegalArgumentException("Cycle detected");
        }
        if (!visited.contains(node)) {
            tempMarked.add(node);
            for (int neighbor : adjacency.get(node)) {
                topologicalSort(adjacency, neighbor, visited, tempMarked, result);
            }
            tempMarked.remove(node);
            visited.add(node);
            result.add(node);
        }
    }
}
