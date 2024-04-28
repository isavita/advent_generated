import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class ConwayCubes {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String line;
        List<String> initialState = new ArrayList<>();
        while ((line = br.readLine()) != null) {
            initialState.add(line);
        }
        br.close();

        Set<String> activeCubes = new HashSet<>();
        for (int y = 0; y < initialState.size(); y++) {
            for (int x = 0; x < initialState.get(y).length(); x++) {
                if (initialState.get(y).charAt(x) == '#') {
                    activeCubes.add(x + "," + y + ",0");
                }
            }
        }

        for (int cycle = 0; cycle < 6; cycle++) {
            activeCubes = nextCycle(activeCubes);
        }

        System.out.println("Active cubes after 6 cycles: " + activeCubes.size());
    }

    private static Set<String> nextCycle(Set<String> activeCubes) {
        Set<String> newActiveCubes = new HashSet<>();
        Set<String> inactiveCubes = new HashSet<>();

        for (String cube : activeCubes) {
            String[] coords = cube.split(",");
            int x = Integer.parseInt(coords[0]);
            int y = Integer.parseInt(coords[1]);
            int z = Integer.parseInt(coords[2]);

            for (int dx = -1; dx <= 1; dx++) {
                for (int dy = -1; dy <= 1; dy++) {
                    for (int dz = -1; dz <= 1; dz++) {
                        if (dx == 0 && dy == 0 && dz == 0) {
                            continue;
                        }
                        int nx = x + dx;
                        int ny = y + dy;
                        int nz = z + dz;
                        String neighbor = nx + "," + ny + "," + nz;
                        if (!activeCubes.contains(neighbor)) {
                            inactiveCubes.add(neighbor);
                        }
                    }
                }
            }
        }

        for (String cube : activeCubes) {
            String[] coords = cube.split(",");
            int x = Integer.parseInt(coords[0]);
            int y = Integer.parseInt(coords[1]);
            int z = Integer.parseInt(coords[2]);
            int activeNeighbors = 0;

            for (int dx = -1; dx <= 1; dx++) {
                for (int dy = -1; dy <= 1; dy++) {
                    for (int dz = -1; dz <= 1; dz++) {
                        if (dx == 0 && dy == 0 && dz == 0) {
                            continue;
                        }
                        int nx = x + dx;
                        int ny = y + dy;
                        int nz = z + dz;
                        String neighbor = nx + "," + ny + "," + nz;
                        if (activeCubes.contains(neighbor)) {
                            activeNeighbors++;
                        }
                    }
                }
            }

            if (activeNeighbors == 2 || activeNeighbors == 3) {
                newActiveCubes.add(cube);
            }
        }

        for (String cube : inactiveCubes) {
            String[] coords = cube.split(",");
            int x = Integer.parseInt(coords[0]);
            int y = Integer.parseInt(coords[1]);
            int z = Integer.parseInt(coords[2]);
            int activeNeighbors = 0;

            for (int dx = -1; dx <= 1; dx++) {
                for (int dy = -1; dy <= 1; dy++) {
                    for (int dz = -1; dz <= 1; dz++) {
                        if (dx == 0 && dy == 0 && dz == 0) {
                            continue;
                        }
                        int nx = x + dx;
                        int ny = y + dy;
                        int nz = z + dz;
                        String neighbor = nx + "," + ny + "," + nz;
                        if (activeCubes.contains(neighbor)) {
                            activeNeighbors++;
                        }
                    }
                }
            }

            if (activeNeighbors == 3) {
                newActiveCubes.add(cube);
            }
        }

        return newActiveCubes;
    }
}