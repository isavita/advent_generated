
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<int[]> robots = new ArrayList<>();
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" ");
                String[] pos = parts[0].substring(2).split(",");
                String[] vel = parts[1].substring(2).split(",");
                int px = Integer.parseInt(pos[0]);
                int py = Integer.parseInt(pos[1]);
                int vx = Integer.parseInt(vel[0]);
                int vy = Integer.parseInt(vel[1]);
                robots.add(new int[]{px, py, vx, vy});
            }

            final int width = 101;
            final int height = 103;
            for (int i = 0; i < 100; i++) {
                for (int[] r : robots) {
                    r[0] = (r[0] + r[2]) % width;
                    r[1] = (r[1] + r[3]) % height;
                    if (r[0] < 0) r[0] += width;
                    if (r[1] < 0) r[1] += height;
                }
            }

            int q1 = 0, q2 = 0, q3 = 0, q4 = 0;
            for (int[] r : robots) {
                int x = r[0];
                int y = r[1];
                if (x == 50 || y == 51) continue;
                if (x < 50 && y < 51) q1++;
                else if (x > 50 && y < 51) q2++;
                else if (x < 50 && y > 51) q3++;
                else q4++;
            }
            System.out.println((long) q1 * q2 * q3 * q4);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
