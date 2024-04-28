import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class PassagePathing {
    private static class Cave {
        String name;
        boolean isBig;
        List<Cave> connections;

        Cave(String name) {
            this.name = name;
            this.isBig = Character.isUpperCase(name.charAt(0));
            this.connections = new ArrayList<>();
        }
    }

    public static void main(String[] args) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            Map<String, Cave> caves = new HashMap<>();
            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split("-");
                Cave cave1 = caves.computeIfAbsent(parts[0], Cave::new);
                Cave cave2 = caves.computeIfAbsent(parts[1], Cave::new);
                cave1.connections.add(cave2);
                cave2.connections.add(cave1);
            }

            Cave start = caves.get("start");
            List<List<Cave>> paths = new ArrayList<>();
            findPaths(start, new ArrayList<>(), paths, caves);

            System.out.println("Number of paths: " + paths.size());
        }
    }

    private static void findPaths(Cave current, List<Cave> path, List<List<Cave>> paths, Map<String, Cave> caves) {
        path.add(current);
        if (current.name.equals("end")) {
            paths.add(new ArrayList<>(path));
        } else {
            for (Cave connection : current.connections) {
                if (connection.isBig || !path.contains(connection)) {
                    findPaths(connection, path, paths, caves);
                }
            }
        }
        path.remove(path.size() - 1);
    }
}