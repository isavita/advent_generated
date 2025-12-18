
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

/**
 * Reactor - Solution for Day 11 Reactor Pathfinding Challenge.
 * This program calculates the number of paths from node 'svr' to 'out' 
 * that pass through both 'dac' and 'fft'.
 */
public class Reactor {
    // Adjacency list representing the network of devices
    private static final Map<String, List<String>> adj = new HashMap<>();
    // Memoization table to cache path counts for specific (start, end) pairs
    private static final Map<String, Long> memo = new HashMap<>();

    public static void main(String[] args) {
        File file = new File("input.txt");
        if (!file.exists()) {
            return;
        }

        try (Scanner sc = new Scanner(file)) {
            // Parsing the input file into an adjacency list
            while (sc.hasNextLine()) {
                String line = sc.nextLine().trim();
                if (line.isEmpty() || !line.contains(":")) continue;

                String[] parts = line.split(":", 2);
                String node = parts[0].trim();
                String[] targets = parts[1].trim().split("\\s+");

                // Populate adjacency list; handle multiple definitions if necessary
                adj.computeIfAbsent(node, k -> new ArrayList<>()).addAll(Arrays.asList(targets));
            }

            /*
             * Problem Logic:
             * We need to find paths from 'svr' to 'out' passing through 'dac' and 'fft'.
             * In a Directed Acyclic Graph (DAG), data cannot "flow backwards", meaning 
             * the visit order must be either (svr -> dac -> fft -> out) or (svr -> fft -> dac -> out).
             * 
             * Total Paths = (Paths svr to dac * Paths dac to fft * Paths fft to out)
             *               + (Paths svr to fft * Paths fft to dac * Paths dac to out)
             */

            long pathOrder1 = countPaths("svr", "dac") * countPaths("dac", "fft") * countPaths("fft", "out");
            long pathOrder2 = countPaths("svr", "fft") * countPaths("fft", "dac") * countPaths("dac", "out");

            System.out.println(pathOrder1 + pathOrder2);

        } catch (FileNotFoundException e) {
            // Silently fail if file cannot be opened as per standard coding challenge constraints
        }
    }

    /**
     * Efficiently counts the number of distinct paths from 'current' to 'target' 
     * using Depth-First Search and Memoization.
     * 
     * @param current The starting node
     * @param target The destination node
     * @return The number of unique paths
     */
    private static long countPaths(String current, String target) {
        // Base case: we have arrived at the destination node
        if (current.equals(target)) {
            return 1;
        }

        // Check the cache to avoid redundant recursive calculations
        String key = current + "->" + target;
        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        long count = 0;
        List<String> neighbors = adj.get(current);
        if (neighbors != null) {
            for (String next : neighbors) {
                count += countPaths(next, target);
            }
        }

        // Store result in cache before returning
        memo.put(key, count);
        return count;
    }
}

