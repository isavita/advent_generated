
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Solution {

    public static void main(String[] args) {
        Map<Integer, List<Integer>> adj = new HashMap<>();
        Map<Integer, Boolean> visited = new HashMap<>();
        int groups = 0;

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" <-> ");
                int from = Integer.parseInt(parts[0]);
                String[] toNodes = parts[1].split(", ");

                for (String toNode : toNodes) {
                    int to = Integer.parseInt(toNode);
                    adj.computeIfAbsent(from, k -> new ArrayList<>()).add(to);
                    adj.computeIfAbsent(to, k -> new ArrayList<>()).add(from);
                }
            }

            for (int node : adj.keySet()) {
                if (!visited.getOrDefault(node, false)) {
                    DFS(node, adj, visited);
                    groups++;
                }
            }

            System.out.println(groups);

        } catch (IOException e) {
            System.out.println("File reading error: " + e.getMessage());
        }
    }

    private static void DFS(int node, Map<Integer, List<Integer>> adj, Map<Integer, Boolean> visited) {
        visited.put(node, true);
        for (int neighbor : adj.getOrDefault(node, new ArrayList<>())) {
            if (!visited.getOrDefault(neighbor, false)) {
                DFS(neighbor, adj, visited);
            }
        }
    }
}
