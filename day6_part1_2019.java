import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

public class solution {

    public static int countOrbits(Map<String, String[]> orbitMap, String start, int depth) {
        String[] orbits = orbitMap.get(start);
        if (orbits == null) {
            return depth;
        }
        int count = depth;
        for (String orbit : orbits) {
            count += countOrbits(orbitMap, orbit, depth + 1);
        }
        return count;
    }

    public static void main(String[] args) {
        try {
            String data = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
            String[] lines = data.split("\n");
            Map<String, String[]> orbitMap = new HashMap<>();
            for (String line : lines) {
                String[] parts = line.split("\\)");
                String center = parts[0];
                String orbiter = parts[1];
                if (!orbitMap.containsKey(center)) {
                    orbitMap.put(center, new String[0]);
                }
                String[] currentOrbits = orbitMap.get(center);
                String[] newOrbits = new String[currentOrbits.length + 1];
                System.arraycopy(currentOrbits, 0, newOrbits, 0, currentOrbits.length);
                newOrbits[currentOrbits.length] = orbiter;
                orbitMap.put(center, newOrbits);
            }

            int totalOrbits = countOrbits(orbitMap, "COM", 0);
            System.out.println(totalOrbits);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}