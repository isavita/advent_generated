import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String input = "";
        String line;
        while ((line = br.readLine()) != null) {
            input += line + "\n";
        }
        br.close();
        input = input.trim();
        System.out.println(solve(input));
    }

    static int solve(String input) {
        String[][] parsed = parseInput(input);
        Map<String, Map<String, Boolean>> graph = new HashMap<>();
        for (String[] pair : parsed) {
            graph.computeIfAbsent(pair[0], k -> new HashMap<>()).put(pair[1], true);
            graph.computeIfAbsent(pair[1], k -> new HashMap<>()).put(pair[0], true);
        }
        return walk(graph, "start", new HashMap<>(), new ArrayList<>(), false);
    }

    static int walk(Map<String, Map<String, Boolean>> graph, String current, Map<String, Integer> visited, List<String> path, boolean doubleUsed) {
        if (current.equals("end")) {
            return 1;
        }
        visited.put(current, visited.getOrDefault(current, 0) + 1);
        int pathsToEnd = 0;
        for (String visitable : graph.get(current).keySet()) {
            if (visitable.equals("start")) {
                continue;
            }
            if (!visitable.equals(visitable.toUpperCase()) && visited.getOrDefault(visitable, 0) > 0) {
                if (doubleUsed) {
                    continue;
                } else {
                    doubleUsed = true;
                }
            }
            path.add(visitable);
            pathsToEnd += walk(graph, visitable, visited, path, doubleUsed);
            visited.put(visitable, visited.getOrDefault(visitable, 0) - 1);
            path.remove(path.size() - 1);
            if (!visitable.equals(visitable.toUpperCase()) && visited.getOrDefault(visitable, 0) == 1) {
                doubleUsed = false;
            }
        }
        return pathsToEnd;
    }

    static String[][] parseInput(String input) {
        String[] lines = input.split("\n");
        List<String[]> parsed = new ArrayList<>();
        for (String line : lines) {
            parsed.add(line.split("-"));
        }
        return parsed.toArray(new String[0][0]);
    }
}