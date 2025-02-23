
import java.io.*;
import java.util.*;

public class Solution {
    private static Map<String, Set<String>> graph = new HashMap<>();
    private static List<String> bestClique = new ArrayList<>();

    public static void main(String[] args) throws IOException {
        Set<String> nodes = new HashSet<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.strip().split("-");
                graph.computeIfAbsent(parts[0], k -> new HashSet<>()).add(parts[1]);
                graph.computeIfAbsent(parts[1], k -> new HashSet<>()).add(parts[0]);
                nodes.add(parts[0]);
                nodes.add(parts[1]);
            }
        }

        bronKerbosch(new ArrayList<>(), new ArrayList<>(nodes), new ArrayList<>());
        Collections.sort(bestClique);
        System.out.println(String.join(",", bestClique));
    }

    private static void bronKerbosch(List<String> r, List<String> p, List<String> x) {
        if (p.isEmpty() && x.isEmpty()) {
            if (r.size() > bestClique.size()) {
                bestClique = new ArrayList<>(r);
            }
            return;
        }

        List<String> pCopy = new ArrayList<>(p);
        for (String v : pCopy) {
            Set<String> neighbors = graph.getOrDefault(v, new HashSet<>());
            List<String> nextR = new ArrayList<>(r);
            nextR.add(v);
            List<String> nextP = new ArrayList<>();
            List<String> nextX = new ArrayList<>();

            for(String n: p){
                if(neighbors.contains(n)) nextP.add(n);
            }

            for(String n: x){
                if(neighbors.contains(n)) nextX.add(n);
            }

            bronKerbosch(nextR, nextP, nextX);
            p.remove(v);
            x.add(v);
        }
    }
}
