
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

/**
 * Reactor Path Finder
 * 
 * This program calculates the number of distinct paths from the 'you' node 
 * to the 'out' node in a directed acyclic graph (DAG). 
 * It uses memoization to ensure efficiency even with complex networks.
 */
public class Reactor {

    private static final String START_NODE = "you";
    private static final String END_NODE = "out";
    
    // Adjacency list to store the graph
    private final Map<String, List<String>> adjacencyList = new HashMap<>();
    // Memoization map to store calculated path counts from a given node to 'out'
    private final Map<String, Long> memo = new HashMap<>();

    public static void main(String[] args) {
        Reactor reactor = new Reactor();
        reactor.solve();
    }

    public void solve() {
        parseInput("input.txt");
        
        // Calculate paths starting from 'you'
        long totalPaths = countPaths(START_NODE);
        
        System.out.println(totalPaths);
    }

    /**
     * Recursively counts paths to the 'out' node using memoization.
     * Time Complexity: O(V + E) where V is nodes and E is edges.
     */
    private long countPaths(String currentNode) {
        // Base case: we reached the target
        if (currentNode.equals(END_NODE)) {
            return 1L;
        }

        // Return cached result if already calculated
        if (memo.containsKey(currentNode)) {
            return memo.get(currentNode);
        }

        long count = 0;
        List<String> neighbors = adjacencyList.get(currentNode);

        if (neighbors != null) {
            for (String neighbor : neighbors) {
                count += countPaths(neighbor);
            }
        }

        // Cache and return
        memo.put(currentNode, count);
        return count;
    }

    /**
     * Reads the input file and builds the adjacency list.
     */
    private void parseInput(String filename) {
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                // Expected format: "node: neighbor1 neighbor2 ..."
                String[] parts = line.split(":");
                if (parts.length < 1) continue;

                String source = parts[0].trim();
                List<String> targets = new ArrayList<>();

                if (parts.length > 1) {
                    String[] neighbors = parts[1].trim().split("\\s+");
                    for (String neighbor : neighbors) {
                        if (!neighbor.isEmpty()) {
                            targets.add(neighbor);
                        }
                    }
                }
                adjacencyList.put(source, targets);
            }
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
    }
}
