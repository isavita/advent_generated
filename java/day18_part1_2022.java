import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class LavaDroplet {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            Set<String> cubes = new HashSet<>();
            String line;
            while ((line = br.readLine()) != null) {
                cubes.add(line);
            }

            int surfaceArea = 0;
            for (String cube : cubes) {
                String[] coordinates = cube.split(",");
                int x = Integer.parseInt(coordinates[0]);
                int y = Integer.parseInt(coordinates[1]);
                int z = Integer.parseInt(coordinates[2]);

                surfaceArea += 6; // each cube has 6 sides

                // subtract sides that are connected to another cube
                if (cubes.contains((x - 1) + "," + y + "," + z)) surfaceArea--;
                if (cubes.contains((x + 1) + "," + y + "," + z)) surfaceArea--;
                if (cubes.contains(x + "," + (y - 1) + "," + z)) surfaceArea--;
                if (cubes.contains(x + "," + (y + 1) + "," + z)) surfaceArea--;
                if (cubes.contains(x + "," + y + "," + (z - 1))) surfaceArea--;
                if (cubes.contains(x + "," + y + "," + (z + 1))) surfaceArea--;
            }

            System.out.println("Surface area: " + surfaceArea);
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }
}