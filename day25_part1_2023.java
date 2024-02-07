
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

class Solution {
    static class Vertice {
        String name;

        Vertice(String name) {
            this.name = name;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Vertice vertice = (Vertice) o;
            return name.equals(vertice.name);
        }

        @Override
        public int hashCode() {
            return name.hashCode();
        }
    }

    static class Edge {
        Vertice start;
        Vertice end;
        int weight;

        Edge(Vertice start, Vertice end, int weight) {
            this.start = start;
            this.end = end;
            this.weight = weight;
        }
    }

    static Map<Vertice, Set<Edge>> parseInput(String[] input) {
        int weight = 1;

        Map<Vertice, Set<Edge>> graph = new HashMap<>();

        for (String line : input) {
            String[] parts = line.split(": ");
            Vertice vertice = new Vertice(parts[0]);
            String[] others = parts[1].split(" ");

            if (!graph.containsKey(vertice)) {
                graph.put(vertice, new HashSet<>());
            }

            for (String other : others) {
                Vertice otherVertice = new Vertice(other);
                if (!graph.containsKey(otherVertice)) {
                    graph.put(otherVertice, new HashSet<>());
                }

                graph.get(vertice).add(new Edge(vertice, otherVertice, weight));
                graph.get(otherVertice).add(new Edge(otherVertice, vertice, weight));
            }
        }

        return graph;
    }

    static class SearchResult {
        boolean found;
        Map<Vertice, Vertice> cameFrom;

        SearchResult(boolean found, Map<Vertice, Vertice> cameFrom) {
            this.found = found;
            this.cameFrom = cameFrom;
        }
    }

    static SearchResult breadthFirstSearch(Map<Vertice, Set<Edge>> graph, Vertice start, Vertice goal, Map<Vertice, Vertice> cameFrom) {
        Set<Vertice> frontier = new HashSet<>();
        Set<Vertice> reached = new HashSet<>();
        frontier.add(start);
        reached.add(start);
        cameFrom.put(start, start);

        while (!frontier.isEmpty()) {
            Vertice current = frontier.iterator().next();
            frontier.remove(current);

            if (current.equals(goal)) {
                return new SearchResult(true, cameFrom);
            }

            for (Edge next : graph.get(current)) {
                if (!reached.contains(next.end)) {
                    frontier.add(next.end);
                    reached.add(next.end);
                    cameFrom.put(next.end, current);
                }
            }
        }

        return new SearchResult(false, cameFrom);
    }

    static Set<Vertice> reconstructPath(Vertice start, Vertice end, Map<Vertice, Vertice> cameFrom) {
        Set<Vertice> path = new HashSet<>();
        Vertice current = end;
        while (!current.equals(start)) {
            path.add(current);
            current = cameFrom.get(current);
        }
        path.add(start);
        return path;
    }

    static Map<Vertice, Set<Edge>> copyGraph(Map<Vertice, Set<Edge>> graph) {
        Map<Vertice, Set<Edge>> newGraph = new HashMap<>();
        for (Map.Entry<Vertice, Set<Edge>> entry : graph.entrySet()) {
            newGraph.put(entry.getKey(), new HashSet<>(entry.getValue()));
        }
        return newGraph;
    }

    static int solve(String[] input) {
        int minCut = 3;

        Map<Vertice, Set<Edge>> graph = parseInput(input);

        Vertice source = null;
        for (Vertice vertice : graph.keySet()) {
            source = vertice;
            break;
        }

        Map<Vertice, Set<Edge>> separateGraph = null;
        for (Vertice end : graph.keySet()) {
            if (source.equals(end)) {
                continue;
            }

            Map<Vertice, Set<Edge>> newGraph = copyGraph(graph);
            for (int i = 0; i < minCut; i++) {
                Map<Vertice, Vertice> cameFrom = new HashMap<>();
                SearchResult result = breadthFirstSearch(newGraph, source, end, cameFrom);
                if (result.found) {
                    Set<Vertice> path = reconstructPath(source, end, cameFrom);
                    for (Vertice vertice : path) {
                        for (Edge edge : new HashSet<>(newGraph.get(vertice))) {
                            if (path.contains(edge.end)) {
                                newGraph.get(vertice).remove(edge);
                            }
                        }
                    }
                }
            }

            Map<Vertice, Vertice> cameFrom = new HashMap<>();
            SearchResult result = breadthFirstSearch(newGraph, source, end, cameFrom);
            if (!result.found) {
                separateGraph = newGraph;
                break;
            }
        }

        Map<Vertice, Vertice> cameFrom = new HashMap<>();
        SearchResult result = breadthFirstSearch(separateGraph, source, new Vertice(""), cameFrom);
        int length1 = cameFrom.size();
        int length2 = separateGraph.size() - length1;

        return length1 * length2;
    }

    static String[] readFile(String fileName) {
        try (BufferedReader reader = new BufferedReader(new FileReader(fileName))) {
            return reader.lines().toArray(String[]::new);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) {
        String[] input = readFile("input.txt");
        System.out.println(solve(input));
    }
}
