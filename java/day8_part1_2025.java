
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;

/**
 * Solution for the Playground challenge.
 * 
 * This program reads junction box coordinates from 'input.txt', identifies 
 * the 1000 closest pairs based on Euclidean distance, connects them to 
 * form electrical circuits, and calculates the product of the sizes of 
 * the three largest circuits formed.
 */
public class Solution {

    /**
     * Represents a junction box at a specific 3D coordinate.
     */
    static class Point {
        int x, y, z;

        Point(int x, int y, int z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
    }

    /**
     * Represents a potential connection between two junction boxes.
     * Implements Comparable to maintain a deterministic sorting order 
     * based on distance and point indices as tie-breakers.
     */
    static class Edge implements Comparable<Edge> {
        int u, v;
        long distSq;

        Edge(int u, int v, long distSq) {
            this.u = u;
            this.v = v;
            this.distSq = distSq;
        }

        @Override
        public int compareTo(Edge other) {
            // Primary sort: Distance squared (shorter distance first)
            if (this.distSq != other.distSq) {
                return Long.compare(this.distSq, other.distSq);
            }
            // Secondary sort: point index u (tie-breaker)
            if (this.u != other.u) {
                return Integer.compare(this.u, other.u);
            }
            // Tertiary sort: point index v (tie-breaker)
            return Integer.compare(this.v, other.v);
        }
    }

    public static void main(String[] args) {
        List<Point> points = new ArrayList<>();
        File inputFile = new File("input.txt");

        if (!inputFile.exists()) {
            return;
        }

        // Read all junction boxes into memory
        try (BufferedReader br = new BufferedReader(new FileReader(inputFile))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;
                String[] parts = line.split(",");
                if (parts.length >= 3) {
                    try {
                        int x = Integer.parseInt(parts[0].trim());
                        int y = Integer.parseInt(parts[1].trim());
                        int z = Integer.parseInt(parts[2].trim());
                        points.add(new Point(x, y, z));
                    } catch (NumberFormatException ignored) {}
                }
            }
        } catch (IOException e) {
            return;
        }

        int n = points.size();
        if (n == 0) return;

        // Use a Max-Heap to efficiently extract the 1000 globally smallest edges.
        // This avoids storing N^2 edges in memory, allowing the program to handle
        // larger inputs while maintaining O(N^2) time complexity for distance calculations.
        int k = 1000;
        PriorityQueue<Edge> pq = new PriorityQueue<>(Collections.reverseOrder());

        for (int i = 0; i < n; i++) {
            Point p1 = points.get(i);
            for (int j = i + 1; j < n; j++) {
                Point p2 = points.get(j);
                
                // Euclidean distance squared: dx^2 + dy^2 + dz^2
                long dx = (long) p1.x - p2.x;
                long dy = (long) p1.y - p2.y;
                long dz = (long) p1.z - p2.z;
                long d2 = dx * dx + dy * dy + dz * dz;

                Edge e = new Edge(i, j, d2);
                
                if (pq.size() < k) {
                    pq.add(e);
                } else if (e.compareTo(pq.peek()) < 0) {
                    pq.poll();
                    pq.add(e);
                }
            }
        }

        // Initialize Disjoint Set Union (DSU) to track connected components (circuits).
        int[] parent = new int[n];
        for (int i = 0; i < n; i++) parent[i] = i;

        // Process the k smallest edges in ascending order of distance.
        List<Edge> topEdges = new ArrayList<>(pq);
        Collections.sort(topEdges);

        for (Edge e : topEdges) {
            int rootU = find(parent, e.u);
            int rootV = find(parent, e.v);
            if (rootU != rootV) {
                // If points are in different circuits, merge them.
                parent[rootU] = rootV;
            }
        }

        // Calculate the sizes of all resulting circuits.
        Map<Integer, Integer> circuitSizes = new HashMap<>();
        for (int i = 0; i < n; i++) {
            int root = find(parent, i);
            circuitSizes.put(root, circuitSizes.getOrDefault(root, 0) + 1);
        }

        // Sort circuit sizes in descending order.
        List<Integer> sizes = new ArrayList<>(circuitSizes.values());
        sizes.sort(Collections.reverseOrder());

        // Multiply the sizes of the three largest circuits.
        long product = 1;
        int countToMultiply = Math.min(3, sizes.size());
        if (sizes.isEmpty()) {
            product = 0;
        } else {
            for (int i = 0; i < countToMultiply; i++) {
                product *= sizes.get(i);
            }
        }

        // Output the final result to standard output.
        System.out.println(product);
    }

    /**
     * Finds the representative root of a component in DSU.
     * Uses path halving for tree height optimization.
     */
    private static int find(int[] parent, int i) {
        while (parent[i] != i) {
            parent[i] = parent[parent[i]]; // Path halving
            i = parent[i];
        }
        return i;
    }
}

