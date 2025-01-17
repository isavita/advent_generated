
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class LANParty {

    public static void main(String[] args) {
        try {
            Map<String, Set<String>> adjList = buildAdjacencyList("input.txt");
            int count = findTrianglesWithT(adjList);
            System.out.println(count);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static Map<String, Set<String>> buildAdjacencyList(String filename) throws IOException {
        Map<String, Set<String>> adjList = new HashMap<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] nodes = line.split("-");
                adjList.computeIfAbsent(nodes[0], k -> new HashSet<>()).add(nodes[1]);
                adjList.computeIfAbsent(nodes[1], k -> new HashSet<>()).add(nodes[0]);
            }
        }
        return adjList;
    }

    private static int findTrianglesWithT(Map<String, Set<String>> adjList) {
        int count = 0;
        List<String> nodes = new ArrayList<>(adjList.keySet());
        for (int i = 0; i < nodes.size(); i++) {
            for (int j = i + 1; j < nodes.size(); j++) {
                for (int k = j + 1; k < nodes.size(); k++) {
                    String u = nodes.get(i);
                    String v = nodes.get(j);
                    String w = nodes.get(k);
                    if (adjList.get(u).contains(v) && adjList.get(v).contains(w) && adjList.get(w).contains(u)) {
                        if (u.startsWith("t") || v.startsWith("t") || w.startsWith("t")) {
                            count++;
                        }
                    }
                }
            }
        }
        return count;
    }
}
