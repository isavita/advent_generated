import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<Disc> discs = new ArrayList<>();
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" ");
                int positions = Integer.parseInt(parts[3]);
                int position = Integer.parseInt(parts[11].replace(".", ""));
                discs.add(new Disc(positions, position));
            }

            int time = 0;
            while (true) {
                boolean success = true;
                for (int i = 0; i < discs.size(); i++) {
                    Disc disc = discs.get(i);
                    if ((disc.position + time + i + 1) % disc.positions != 0) {
                        success = false;
                        break;
                    }
                }
                if (success) {
                    System.out.println("Part 1: " + time);
                    break;
                }
                time++;
            }

            // Add the new disc
            discs.add(new Disc(11, 0));

            time = 0;
            while (true) {
                boolean success = true;
                for (int i = 0; i < discs.size(); i++) {
                    Disc disc = discs.get(i);
                    if ((disc.position + time + i + 1) % disc.positions != 0) {
                        success = false;
                        break;
                    }
                }
                if (success) {
                    System.out.println("Part 2: " + time);
                    break;
                }
                time++;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static class Disc {
        int positions;
        int position;

        Disc(int positions, int position) {
            this.positions = positions;
            this.position = position;
        }
    }
}