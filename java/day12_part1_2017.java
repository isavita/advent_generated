import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class DigitalPlumber {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            Map<Integer, Set<Integer>> graph = new HashMap<>();
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" <-> ");
                int program = Integer.parseInt(parts[0]);
                String[] connections = parts[1].split(", ");
                Set<Integer> connectedPrograms = new HashSet<>();
                for (String connection : connections) {
                    connectedPrograms.add(Integer.parseInt(connection));
                }
                graph.put(program, connectedPrograms);
            }

            Set<Integer> visited = new HashSet<>();
            dfs(graph, 0, visited);
            System.out.println("Number of programs in the group that contains program ID 0: " + visited.size());
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static void dfs(Map<Integer, Set<Integer>> graph, int program, Set<Integer> visited) {
        if (visited.contains(program)) {
            return;
        }
        visited.add(program);
        for (int connectedProgram : graph.get(program)) {
            dfs(graph, connectedProgram, visited);
        }
    }
}