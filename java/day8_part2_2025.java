
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Solution for Day 8: Playground - Part Two
 * 
 * The challenge asks us to continue connecting the closest unconnected pairs of
 * junction boxes until all boxes form a single connected circuit. This is a classic
 * application of finding the Minimum Spanning Tree (MST).
 * 
 * We use Kruskal's algorithm:
 * 1. Generate all possible unique pairs (edges) and calculate their squared Euclidean distance.
 * 2. Sort the edges by distance.
 * 3. Use Disjoint Set Union (DSU) to efficiently track and merge connected components.
 * 4. The final edge that merges the last two components into one is our answer.
 */
public class Playground {

    // Helper class to store 3D coordinates of each junction box
    static class Box {
        long x, y, z;

        Box(long x, long y, long z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
    }

    // Helper class to represent a potential connection between two boxes
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
            // Compare squared distances to avoid expensive square root operations
            return Long.compare(this.distSq, other.distSq);
        }
    }

    // Disjoint Set Union (DSU) with Path Compression to manage circuits
    static class DSU {
        int[] parent;
        int componentCount;

        DSU(int n) {
            parent = new int[n];
            for (int i = 0; i < n; i++) {
                parent[i] = i;
            }
            componentCount = n;
        }

        // Iterative find with path compression to prevent stack overflow on deep trees
        int find(int i) {
            int root = i;
            while (parent[root] != root) {
                root = parent[root];
            }
            while (parent[i] != root) {
                int next = parent[i];
                parent[i] = root;
                i = next;
            }
            return root;
        }

        // Returns true if u and v were in different components and are now merged
        boolean union(int u, int v) {
            int rootU = find(u);
            int rootV = find(v);
            if (rootU != rootV) {
                parent[rootU] = rootV;
                componentCount--;
                return true;
            }
            return false;
        }
    }

    public static void main(String[] args) {
        List<Box> boxes = new ArrayList<>();

        // Read junction box positions from input.txt
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;
                String[] coords = line.split(",");
                if (coords.length >= 3) {
                    try {
                        long x = Long.parseLong(coords[0].trim());
                        long y = Long.parseLong(coords[1].trim());
                        long z = Long.parseLong(coords[2].trim());
                        boxes.add(new Box(x, y, z));
                    } catch (NumberFormatException e) {
                        // Skip any lines that do not contain valid coordinates
                    }
                }
            }
        } catch (IOException e) {
            // Silent catch to meet standard challenge output requirements
            return;
        }

        int n = boxes.size();
        if (n < 2) return;

        // Step 1: Generate all possible unique pairs of junction boxes
        int edgeTotal = n * (n - 1) / 2;
        Edge[] edges = new Edge[edgeTotal];
        int edgeIdx = 0;
        for (int i = 0; i < n; i++) {
            Box b1 = boxes.get(i);
            for (int j = i + 1; j < n; j++) {
                Box b2 = boxes.get(j);
                
                // Euclidean distance squared: (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2
                long dx = b1.x - b2.x;
                long dy = b1.y - b2.y;
                long dz = b1.z - b2.z;
                long d2 = dx * dx + dy * dy + dz * dz;
                
                edges[edgeIdx++] = new Edge(i, j, d2);
            }
        }

        // Step 2: Sort all edges by distance (shortest first)
        Arrays.sort(edges);

        // Step 3: Kruskal's algorithm logic to find the bridging connection
        DSU dsu = new DSU(n);
        for (Edge edge : edges) {
            // Try to connect the two junction boxes in this pair
            if (dsu.union(edge.u, edge.v)) {
                // Check if this connection merged the last two circuits into one
                if (dsu.componentCount == 1) {
                    // Multiply the X coordinates of the last two boxes connected
                    long result = boxes.get(edge.u).x * boxes.get(edge.v).x;
                    System.out.println(result);
                    return;
                }
            }
        }
    }
}

